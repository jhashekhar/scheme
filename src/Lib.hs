module Lib (readExpr, eval) where

import Control.Monad ( liftM )

import Text.Parsec.Prim
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.String ( Parser )
import Text.Parsec.Combinator (many1, notFollowedBy, sepBy, endBy)
import Numeric (readHex, readOct, readFloat)

-- parsers will convert input into this data structure such that traversals become easier
data LispVal 
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    
-- define a parser that recognizes one of the symbols allowed in scheme identifiers
-- parser takes in a String as an input and outputs a value and rest of the unparsed string
-- type Parser = Parsec String ()
-- oneOf takes in [Char] that matches if the current character is in the list of supplied character
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- parse spaces
spaces :: Parser ()
spaces = skipMany space

-- parse String
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

-- escaped Characters
escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
            '\\' -> x
            '"'  -> x
            'n'  -> '\n'
            'r'  -> '\r'
            't'  -> '\t'

-- parse Atom
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
                "t" -> Bool True
                "f" -> Bool False
                _    -> Atom atom

parseStringq :: Parser LispVal
parseStringq = do
    string "\""
    x <- many1 letter
    string "\""
    return $ String x


-- parse Number
parseNumber :: Parser LispVal
parseNumber = parseHex <|> parseOct <|> parseBin <|> parseDecimal1 <|> parseDecimal2

parseHex :: Parser LispVal
parseHex = do 
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head $ readHex x

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ head $ readOct x

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

bin2dig :: [Char] -> Integer
bin2dig  = bin2dig' 0

bin2dig' :: Num t => t -> [Char] -> t
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

-- two way to express decimal is scheme one with #d as prefix and another 
-- like regular number with no prefix or anything.
parseDecimal1 :: Parser LispVal
parseDecimal1 = do
    string "#d"
    val <- many1 digit 
    return $ Number . read $ val

parseDecimal2 :: Parser LispVal
parseDecimal2 = many1 digit >>= return . Number . read 

-- parse chracter 
parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    cinp <- try (string "newline" <|> string "space") <|>
                    do {x <- anyChar; notFollowedBy alphaNum ; return [x]} 
    return $ case cinp of 
        "newline"  -> Character '\n'
        "space"    -> Character ' '
        _          -> Character $ head cinp 

parseFloat :: Parser LispVal
parseFloat = do
    bd <- many1 digit
    char '.'
    ad <- many1 digit
    return $ Float $ fst . head . readFloat $ (bd ++ "." ++ ad) 

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail 
    

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    val <- parseExpr
    return $ List [Atom "quote", val]


-- parser choices
parseExpr :: Parser LispVal
parseExpr = try parseAtom 
         <|> try parseString
         <|> parseStringq
         <|> try parseFloat
         <|> try parseNumber
         <|> parseQuoted
         <|> try parseCharacter 
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
                 

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ show contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal 

instance Show LispVal where show = showVal



-- define a function to call our parser and handle any possible errors
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                    Left err -> String $ "No match: " ++ show err
                    Right val -> val


-- EVALUATOR
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val 
eval (List [Atom "quote", val]) = val



{- EXPERIMENT SECTION -}

-- These are test functions that I've written to clear out certain confusion
-- or be sure about certain ideas. It may be deleted in future updates. Thanks!

-- inspecting change in state of the parser
changeState :: Parsec String Int String
changeState = do
    char 'h'
    s <- getState
    modifyState (+1)
    return $ show s

-- inspecting the consumption of input streams in ParsecT
checkInput :: Parsec String () String
checkInput = do
    char 'h'
    getInput

-- inspect the parsing for single character
parseSingleChar :: Parser LispVal
parseSingleChar = do
    x <- anyChar
    notFollowedBy alphaNum 
    return $ Character x 

parseString1 :: Parser Char
parseString1 = char '"' >> many (noneOf "\"") >> char '"' 

parseString2 :: Parser Char
parseString2 = char '"' >>= (\_ -> many (noneOf "\"") >>= (\_ -> char '"'))

parseStringR5RS :: Parser LispVal
parseStringR5RS = do
    char '"'
    x <- many (alphaNum <|> oneOf "\"\\" <|> newline <|> crlf <|> tab)
    char '"'
    return $ String x

parserNumber1 :: Parser LispVal
parserNumber1 = do
    str <- many1 digit
    return $ (Number . read) str 

parseNumber2 :: Parser LispVal
parseNumber2 = many1 digit >>= return . Number . read
