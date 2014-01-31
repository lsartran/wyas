import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal =  Atom String
            |   List [LispVal]
            |   DottedList [LispVal] LispVal
            |   Number Integer
            |   String String
            |   Bool Bool
            deriving (Show)

charToString :: Char -> String
charToString = flip (:) []

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseStringElement :: Parser String
parseStringElement = do
    try (string "\\\"") <|> try (string "\\\\") <|> (liftM charToString $ noneOf "\"")

parseString :: Parser LispVal
parseString = do
    string "\""
    x <- (liftM concat) $ many parseStringElement
    string "\""
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match:" ++ show err
    Right val -> "Found value:" ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ args !! 0