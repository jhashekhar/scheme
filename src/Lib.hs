module Lib
    ( someFunc
    ) where

import Text.Parsec.Combinator (skipMany1, many1)
import Text.ParserCombinators.Parsec ( oneOf, Parser, parse, space, noneOf, char, many, letter, digit, (<|>) )
import System.Environment.Blank (getArgs)
import Text.Read (reset)
import Control.Monad (liftM)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> (return . Number . read) x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"


someFunc :: IO ()
someFunc = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr

