module Main where
import IO hiding(try)
import Monad
import Data.Char (isSymbol, isPunctuation, isSpace)
import Data.List (concat isPrefixOf findIndex)
import System.Console.GetOpt
import Test.QuickCheck

import Text.ParserCombinators.Parsec hiding (Parser)
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

type Parser a = GenParser Char ParserState a

data ParsedLine = 
  ParsedLine {
    parsedIndent :: String,
    parsedLine :: [Element],
    parsedElements :: [Element]
  }
  deriving Show

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case runParser parseModule () "stdin" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseModule :: Parser Element
parseModule = do 
    list <- parseElements "" []
    spaces
    eof
    return $ list

parseElements :: String -> [Element] Parser Element
parseElements parentIndex elements = do
  indent <- spacesLine
  line <- parseListElementsLine
  eol
  parsed <- parseElementsRec parentIndent elements indent line
  return $ List $ parsedElements parsed

parseElementsRec :: String -> [Element] -> String -> [Element] -> Parser (ParsedLine)
parseElementsRec parentIndent elements indent line = do
  case line of
    (_:[]) ->
      if parentIndent == indent then do
        parsed <- parseElementsRec indent line
        return ParsedLine{
          parsedLine=line, 
          parsedIndent=indent, 
          parsedElements=parsedElements parsed}
      else if parentIndent `isPrefixOf` indent then do
        parsed <- parseElementsRec indent elements
        case childElements of
          (_:_:[]) -> List childElements
          (e:[]) -> e
          otherwise -> fail "Bad parse"
      else if length indent < length parentIndent && 
          not indent `isPrefixOf` parentIndent then
        fail "Bad parse"
      else 
        return ParsedLine{indent=indent, line=line, elements=elements}
    -- if line is empty, ignore indentation and skip to next iteration
    otherwise -> parseElements parentIndent parentElements

eol :: Parser ()
eol = char '\n' <|> (try $ string "\r\n")

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseQuotedString :: Parser String
parseQuotedString = do
    char '\''
    string <- many $ noneOf "'" <|> try (string "''" >> return '\'')
    char '\''
    return string

parseQuoted :: Parser Element
parseQuoted = do
      string <- parseQuotedString
      return $ String string
  <?> "Single Quoted String"

parseDoubleQuotedString :: Parser String
parseDoubleQuotedString = do
    char '"'
    string <- many $ noneOf "\"" <|> try (string "\"\"" >> return '"')
    char '"'
    return string

parseDoubleQuoted :: Parser Element
parseDoubleQuoted = do
      string <- parseDoubleQuotedString
      return $ String string
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
    comment <- parseBalancedParens ('(',')')
      <|> parseBalancedParens ('{','}')
      <|> parseBalancedParens ('[',']')
      <|> parseQuotedString
      <|> parseDoubleQuotedString
      <|> do 
          spaces
          manyTill anyChar (try (
                do string "\r\n" 
                   return ()
            <|> do string "\n"
                   return ()
            <|> eof))
    return $ Comment comment
  <?> "Comment"

parseBalancedParens :: (Char,Char) -> Parser String
parseBalancedParens pair = do
    char (fst pair)
    comment <- (many ((many1 (noneOf [(fst pair),(snd pair)])) 
               <|> do
                   c <- parseBalancedParens pair
                   return $ [fst pair] ++ c ++ [snd pair]))
    let combined = concat comment
    char (snd pair)
    return combined
  <?> "Parenthesized Comment"

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

spaceLine :: CharParser st Char
spaceLine = satisfy (\c -> c /= '\n' && c /= '\r' && isSpace(c))

spacesLine :: Parser String
spacesLine = many spaceLine

spacesLine1 :: Parser String
spacesLine1 = many1 spaceLine

parseListElementsLine :: Parser [Element]
parseListElementsLine = do
  clusters <- sepEndBy parseElementCluster spacesLine1
  let catted = concat clusters
  return catted

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

