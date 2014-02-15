module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
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
             | Nil
             | Func {params :: [String], body :: [LispVal]} --, closure :: Env}
type Env = IORef [(String, IORef LispVal)]
nullEnv :: IO Env
nullEnv = newIORef []

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then return Nil
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
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

makeFunc params body = Func (map showVal params) body
makeNormalFunc = makeFunc

eval :: Env -> LispVal -> IO LispVal
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env (List [Atom "define", Atom var, form]) = do
    val <- defineVar env var form
    return val
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

main :: IO ()
main = do
    line <- getLine
    env <- nullEnv
    val <- eval env (readExpr line)
    print (showVal val)
    main
