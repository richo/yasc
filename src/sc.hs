{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Error
import Data.IORef

symbol :: Parser Char
symbol = oneOf "!#$%^|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Func {params :: [String], body :: [LispVal], closure :: Env}

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                            ++ " args; found values " ++ unwordsList found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

runThrows :: ThrowsError String -> String
runThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> ThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "reference to unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> ThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "set unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> ThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

makeFunc env params body = return $ Func (map showVal params) body env
makeNormalFunc = makeFunc Nothing

escapedinnerstring :: Parser Char
escapedinnerstring = do
                     c <- anyChar
                     return $ case c of
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'
                        '"'  -> '"'
                        other->other

innerstring :: Parser Char
innerstring = do
              c <- anyChar
              case c of
                '\\'    -> escapedinnerstring
                other   -> return other

parseString :: Parser LispVal
parseString = do char '"' -- Read until we find this char
                 x <- manyTill innerstring $ char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest -- : list cons
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom -- _ == .* for pattern matching

parseNumber :: Parser LispVal
parseNumber = do digits <- many1 digit
                 return $ Number (read digits)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                  head <- endBy parseExpr spaces
                  tail <- char '.' >> spaces >> parseExpr
                  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
              char '\''
              x <- parseExpr
              return $ List [Atom "quote", x]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Func {params = args, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ ") ... )"



-- Only valid toplevel form is (define name value)
-- At some point (export value) needs to work too
topLevelEval :: Env -> LispVal -> ThrowsError LispVal
topLevelEval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
topLevelEval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

evalExpressions :: Env -> [LispVal] -> ThrowsError LispVal
evalExpressions env [expr] = eval env expr
evalExpressions env (expr: expressions) = do
    eval env expr
    evalExpressions env expressions

eval :: Env -> LispVal -> ThrowsError LispVal
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env (Atom id)                  = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List (Atom "begin": expressions)) = do evalExpressions env expressions
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
-- eval env (List [Atom "load", String filename]) =
--     load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (Func params body closure) args =
    if length params /= length args
         then throwError $ NumArgs (length params) args
         -- else (bindVars closure $ zip params args) >>= evalBody
         else String "<applied func>"



    -- where remainingArgs = drop (length params) args
    --       num          = toInteger . length
    --       evalBody env = liftM last $ mapM (eval env) body
    --       bindVarArgs arg env = case arg of
    --           Just argName  -> bindVars env [(argName, List $ remainingArgs)]
    --           Nothing       -> return env


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

runOne :: [String] -> IO ()
runOne args = do
    env <- nullEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
         >>= hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    runOne $ args
