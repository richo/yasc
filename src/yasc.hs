module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Error
import Data.IORef
import qualified Data.Map as M

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
             | Nil
             | Func {params :: [String], body :: [LispVal]} --, closure :: Env}
             | Intrinsic {params :: [String], body :: [LispVal]}
type InnerEnv = M.Map String (IORef LispVal)
type Env = IORef InnerEnv

nullEnv :: IO Env
nullEnv = newIORef M.empty

parseString :: Parser LispVal
parseString = do char '"' -- Read until we find this char
                 x <- many (noneOf "\"")
                 char '"'
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

load :: String -> IO [LispVal]
load filename = (liftIO $ readFile filename) >>= readExprList

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- readIORef envRef
    case M.lookup var env of
        Just _   -> return True
        Nothing  -> return False

getVar :: Env -> String -> IO LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (return Nil)
                             (liftIO . readIORef)
                             (M.lookup var env)

setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             valueRef <- newIORef value
                             env <- readIORef envRef
                             writeIORef envRef (M.insert var valueRef env)
                             return value

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then return Nil -- TODO Barf instead of silently doing nothing
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef (M.insert var valueRef env)
            return value


showVal :: LispVal -> String
showVal Nil                    = "'nil"
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Func {params = args, body = body}) =
    "(lambda (" ++ unwords (map show args) ++ ") ... )"
showVal (Intrinsic {params = args, body = body}) =
    "(intrinsic (" ++ unwords (map show args) ++ ") ... )"

makeFunc params body = Func (map showVal params) body
makeNormalFunc = makeFunc
makeIntrinsic params body = Intrinsic (map showVal params) body

withNewExport :: LispVal -> LispVal -> LispVal
withNewExport (List contents) (String new) = List ( String new : contents )

exportName :: Env -> String -> IO LispVal
exportName env name = do
    exports <- getVar env "exports"
    setVar env "exports" $ withNewExport exports (String name)

topLevelEval :: Env -> LispVal -> IO LispVal
-- TODO Consider making this support some of:
-- (intrinsic! (foo bar) ...)
-- (intrinsic! (foo bar . rest) ...)
topLevelEval env (List (Atom "intrinsic!" : Atom name : List params : body)) =
    defineVar env name $ makeIntrinsic params body
topLevelEval env (List [Atom "export", Atom name]) =
    -- TODO Should also barf if you try to export undefined names
    liftIO $ exportName env name
topLevelEval env (List [Atom "define", Atom var, form]) = do
    ret <- eval env form
    val <- defineVar env var ret
    return val
topLevelEval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (topLevelEval env)
-- TODO Remove, just for debugging
topLevelEval env (Atom id)                  = getVar env id


eval :: Env -> LispVal -> IO LispVal
eval env (Atom id)                  = getVar env id
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "lambda" : List params : body)) =
    return $ makeNormalFunc params body


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", typePredicate isAtom),
              ("symbol->string", atomToString),
              ("string->symbol", stringToAtom),
              ("string?", typePredicate isString),
              ("number?", typePredicate isNumber)]

typePredicate :: (LispVal -> Bool) -> [LispVal] -> LispVal
typePredicate predicate params = Bool $ predicate $ head params

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

isAtom :: LispVal -> Bool
isAtom (Atom _) = True
isAtom _        = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

atomToString :: [LispVal] -> LispVal
atomToString params = case head params of
                    (Atom n) -> String n
                    _        -> Bool False

stringToAtom :: [LispVal] -> LispVal
stringToAtom params = case head params of
                    (String n) -> Atom n
                    _          -> Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

instance Show LispVal where show = showVal


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "scheme" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

readExprList :: String -> IO [LispVal]
readExprList input = case parse (endBy parseExpr spaces) "scheme" input of
    Left err  -> do
        return [Nil] -- Again, stop dumping nils everywhere
    Right val -> return val

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

emitAll :: Env -> LispVal -> IO [()]
emitAll env (List names) = mapM (emit env) names

emit :: Env -> LispVal -> IO ()
emit env (String name) = do
    val <- getVar env name
    print $ show val

runOne :: [String] -> Env -> IO ()
runOne args env = do
    (liftM show $ topLevelEval env (List [Atom "load", String (args !! 0)]))
    exports <- (getVar env "exports")
    emitAll env exports
    hFlush stdout

runRepl :: Env -> IO ()
runRepl env = do
    putStr "yasc> "
    hFlush stdout

    line <- getLine
    val <- topLevelEval env (readExpr line)
    print (showVal val)
    runRepl env

main :: IO ()
main = do
    env <- nullEnv
    defineVar env "exports" $ List []
    args <- getArgs
    if null args then runRepl env else runOne args env
