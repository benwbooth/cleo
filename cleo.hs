module Main where
import IO hiding(try)
import Monad
import Data.Char (isSymbol, isPunctuation)
import Data.List (concat)
import System.Console.GetOpt

import Text.ParserCombinators.Parsec
import System.Environment.UTF8
import System.Console.Readline
import Data.Decimal
import LLVM.Core
import LLVM.ExecutionEngine

data Element = Symbol String
  | Operator String
  | PrefixOperator String
  | PostfixOperator String
  | List [Element]
  | SquareBracketList [Element]
  | CurlyBraceList [Element]
  | Integer String
  | Float String
  | Binary String
  | Hexadecimal String
  | Octal String
  | String String
  | DoubleString String
  | Backtick String
  | Comment String
  deriving Show

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "Prototype Language" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseQuoted :: Parser Element
parseQuoted = do
    char '\''
    x <- many $ noneOf "'" <|> try (string "''" >> return '\'')
    char '\''
    return $ String x
  <?> "Single Quoted String"

parseDoubleQuoted :: Parser Element
parseDoubleQuoted = do
    char '"'
    x <- many $ noneOf "\"" <|> try (string "\"\"" >> return '"')
    char '"'
    return $ DoubleString x
  <?> "Double Quoted String"

parseSymbol :: Parser Element
parseSymbol = do
    first <- letter <|> char '_'
    rest <- many (letter <|> digit <|> char '_')
    let symbol = first : rest
    return $ Symbol symbol
  <?> "Symbol"

parseOperatorString :: Parser String
parseOperatorString = do 
    operator <- parseBacktickQuoted 
      <|> (many1 (satisfy (\c -> 
          c /= '(' && c /= ')' && 
          c /= '{' && c /= '}' && 
          c /= '[' && c /= ']' && 
          c /= '\'' && c /= '"' && c /= '`' &&
          c /= '#' && c /= '_' &&
          (isSymbol c || isPunctuation c))) <?> "Operator")
    return operator
  <?> "Operator"

parseBacktickQuoted :: Parser String
parseBacktickQuoted = do
    char '`'
    x <- many $ noneOf "`" <|> try (string "``" >> return '`')
    char '`'
    return x
  <?> "Backquoted Operator"

parseComment :: Parser Element
parseComment = do
    string "#"
    spaces
    comment <- manyTill anyChar (try (
          do string "\r\n" 
             return ()
      <|> do string "\n"
             return ()
      <|> eof))
    return $ Comment comment
  <?> "Comment"

parseList :: Parser Element
parseList = do
    char '('
    spaces
    x <- parseListElements
    char ')'
    return $ List x
  <?> "List"

parseSquareBracketList :: Parser Element
parseSquareBracketList = do
    char '['
    spaces
    x <- parseListElements
    char ']'
    return $ SquareBracketList x
  <?> "Bracketed List"

parseCurlyBraceList :: Parser Element
parseCurlyBraceList = do
    char '{'
    spaces
    x <- parseListElements
    char '}'
    return $ CurlyBraceList x
  <?> "Curly Brace List"

parseListElements :: Parser [Element]
parseListElements = do
  clusters <- sepEndBy parseElementCluster spaces1
  let catted = concat clusters
  return catted

parseElementCluster :: Parser [Element]
parseElementCluster = parsePrefixInfix <|> parseFirstItem

parsePrefixInfix :: Parser [Element]
parsePrefixInfix = do
  op <- parseOperatorString 
  item <- option [] parseItemAsList
  case item of
    (_:[]) -> do
      more <- many parseInfixPostfix
      return $ PrefixOperator op : (item ++ (concat more))
    otherwise -> do 
      return $ [Operator op]

parseFirstItem :: Parser [Element]
parseFirstItem = do
  item <- parseItemAsList
  more <- many parseInfixPostfix
  return $ item ++ (concat more)

parseInfixPostfix :: Parser [Element]
parseInfixPostfix = do
  op <- parseOperatorString
  item <- option [] parseItemAsList
  return $ case item of
    (_:[]) -> Operator op : item
    otherwise -> [PostfixOperator op]

parseExpr :: Parser Element
parseExpr = parseItem

parseItem :: Parser Element
parseItem = parseQuoted 
      <|> parseDoubleQuoted 
      <|> parseNumber
      <|> parseList
      <|> parseSquareBracketList
      <|> parseCurlyBraceList
      <|> parseComment
      <|> parseSymbol

parseItemAsList :: Parser [Element]
parseItemAsList = do
  item <- parseItem
  return [item]

-- Numeric literals

parseNumber :: Parser Element
parseNumber = do 
        -- parse a 0 then the number
        char '0'
        num <- zeroNumFloat
        return num
    -- or parse a decimal/float
    <|> decimalFloat ""
  <?> "Number"
                  
zeroNumFloat :: Parser Element
zeroNumFloat =  do 
      -- try to parse an integer in hex/octal/binary format
      hexadecimal
  <|> octal 
  <|> binary
  -- or parse a decimal/float, e.g. 1.2e10
  <|> decimalFloat "0"
  -- or parse a fract/float, e.g. .2e10
  <|> fractFloat "0"
  -- if all that fails this must be "just" a 0
  <|> return (Integer "0")
  
decimalFloat :: String -> Parser Element
decimalFloat zero = do 
  -- try to parse a decimal
  num <- decimal
  let znum = zero ++ num
  -- see if there's a trailing fractional component
  option (Integer num) (fractFloat znum)

fractFloat :: String -> Parser Element
fractFloat num = do 
    -- parse the fractional part
    char '.'
    fract <- many1 digit
    exp <- option "" exponent'
    return $ Float $ num ++ "." ++ fract ++ exp
  <|>
  -- just parse the exponent part (e.g. 0e-10)
  do exp <- exponent'
     return $ Float (num ++ exp)

exponent' :: Parser String
exponent' = do
    e <- oneOf "eE"
    s <- sign
    exp <- decimal <?> "exponent"
    return $ e : s ++ exp
  <?> "exponent"

sign :: Parser String
sign = option ("") (string "-" <|> string "+") <?> "Exponent Sign"

decimal :: Parser String
decimal = many1 digit 

hexadecimal :: Parser Element
hexadecimal = do
    x <- oneOf "xX"
    hex <- many1 hexDigit
    return $ Hexadecimal ('0' : x : hex)
  <?> "Hexadecimal Integer"

octal :: Parser Element
octal = do 
    o <- oneOf "oO"
    oct <- many1 octDigit
    return $ Octal ('0' : o : oct)
  <?> "Octal Integer"

binary :: Parser Element
binary = do 
    b <- oneOf "bB"
    bin <- many1 binaryDigit
    return $ Binary ('0' : b : bin)
  <?> "Binary Integer"

-- parse a binary digit
binaryDigit :: Parser Char
binaryDigit = satisfy (\c -> c == '0' || c == '1') <?> "binary digit"

