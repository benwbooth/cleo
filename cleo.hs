module Main where
import IO hiding(try)
import Monad
import Data.Char (isSymbol, isPunctuation, isSpace)
import Data.List (concat, isPrefixOf, findIndex)
import System.Console.GetOpt

--import Test.QuickCheck
import Text.ParserCombinators.Parsec
import System.Environment.UTF8
--import System.Console.Readline
--import Data.Decimal
--import LLVM.Core
--import LLVM.ExecutionEngine

-- tokens
data Item = Symbol String
  | Operator String
  | PrefixOperator String
  | PostfixOperator String
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

-- block delimiter types
data Delim = Parens
  | Square
  | Curly
  | Indent
  deriving Show

-- tokens + delimiters
data Token = Token Item
  | Begin Delim
  | End Delim
  deriving Show

-- parsed elements
data Element = Element Item
  | List [Element]
  | SquareBracketList [Element]
  | CurlyBraceList [Element]
  deriving Show

type TokenParser a = GenParser Char () a
type Parser a = GenParser Token () a

-- Top-level parser functions

main :: IO ()
main = do
  args <- getArgs
  let filename = args !! 0
  parseFile filename

parseFile :: String -> IO ()
parseFile filename = do
  --putStrLn (readExpr (args !! 0) "input")
  str <- readFile filename
  putStrLn (readExpr str filename)

readExpr :: String -> String -> String
readExpr input inputName =
  let tokens = case runParser parseTokens () inputName (input ++ "\n") of
      Left err -> fail $ "No match: " ++ show err
      Right val -> val
  in case runParser parseModule () inputName tokens of
      Left err -> "No match: " ++ show err
      Right val -> "Found value: " ++ show val

-- Tokenizer Functions
--
satisfyToken :: (Token -> Maybe a) -> TokenParser a
satisfyToken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

tokenEquals :: TokenParser a
tokenEquals = satisfyToken (==)

parseTokens :: TokenParser [Token]
parseTokens = parseLines [] [""] 

-- TODO: There needs to be a "many" here somewhere
parseLines :: [Token] -> [String] -> TokenParser [Token]
parseLines tokens indentStack = 
  parseLiterateComment tokens indentStack
  <|> parseTokenLines tokens indentStack

-- Unindented lines are literate comments
parseLiterateComment :: [Token] -> [String] -> TokenParser [Token]
parseLiterateComment tokens indentStack = do
    lines <- many1 parseLiterateCommentLine
    return $ parseLines (tokens ++ Comment (concat lines)) indentStack
  <?> "Literate comment"

parseLiterateCommentLine :: TokenParser String
parseLiterateCommentLine = do
      -- make sure this line is unindented
      notFollowedBy spaceLine
      line <- manyTill anyChar eol
      return $ line ++ "\n"
      <?> "Literate comment line"

parseTokenLines :: [Token] -> [String] -> TokenParser [Token]
parseTokenLines tokens indentStack = do
  indent <- spacesLine1
  elements <- parseListElementsLine
  case elements of
    (_:[]) -> case getIndentTokens indentStack indent of
                Right (indentTokens, updatedStack) ->
                  parseLines (indentTokens ++ elements) updatedStack
                Left err -> fail err
    otherwise -> parseLines tokens indentStack

getIndentTokens :: [String] -> String -> Either String ([Token], [String])
getIndentTokens indentStack indent = 
  let (dedents, updatedStack) = break (\i -> indent `isPrefixOf` i && indent /= i) indentStack in
    if head updatedStack == indent then
      Right (replicate (length dedents) (End Indent), updatedStack)
    else if head updatedStack `isPrefixOf` indent then
      Right (replicate (length dedents) (End Indent) ++ [Begin Indent], indent:updatedStack)
    else
      Left "Inconsistent whitespace usage in indentation"

eol :: TokenParser String
eol = string "\n" 
    <|> do
      a <- string "\r"
      b <- option "" (string "\n")
      return $ a ++ b
  <?> "eol"

spaces1 :: TokenParser ()
spaces1 = skipMany1 space

-- TODO: Add support for multiline indented string literals
parseQuotedString :: TokenParser String
parseQuotedString = do
    char '\''
    string <- many $ noneOf "'\n" <|> try (string "''" >> return '\'')
    char '\''
    return string

parseQuoted :: TokenParser Token
parseQuoted = do
      string <- parseQuotedString
      return $ Token $ String string
  <?> "Single Quoted String"

parseDoubleQuotedString :: TokenParser String
parseDoubleQuotedString = do
    char '"'
    string <- many $ noneOf "\"\n" <|> try (string "\"\"" >> return '"')
    char '"'
    return string

parseDoubleQuoted :: TokenParser Token
parseDoubleQuoted = do
      string <- parseDoubleQuotedString
      return $ Token $ String string
  <?> "Double Quoted String"

parseSymbol :: TokenParser Token
parseSymbol = do
    first <- letter <|> char '_'
    rest <- many (letter <|> digit <|> char '_' <|> char '?' <|> char '!')
    let symbol = first : rest
    return $ Token $ Symbol symbol
  <?> "Symbol"

parseOperatorString :: TokenParser String
parseOperatorString = do 
    operator <- parseBacktickQuoted 
      <|> (many1 (satisfy (\c -> 
          c /= '(' && c /= ')' && 
          c /= '{' && c /= '}' && 
          c /= '[' && c /= ']' && 
          c /= '\'' && c /= '"' && c /= '`' &&
          c /= '_' && 
          (isSymbol c || isPunctuation c))) <?> "Operator")
    return operator
  <?> "Operator"

parseBacktickQuoted :: TokenParser String
parseBacktickQuoted = do
    char '`'
    x <- many $ noneOf "`\n" <|> try (string "``" >> return '`')
    char '`'
    return x
  <?> "Backquoted Operator"

--TODO: Comments appearing after other elements on the same line
--should be marked as "PostComment"
parseComment :: TokenParser Token
parseComment = do
    try $ string "\\\\"
    comment <- manyTill anyChar eol
    return $ Token $ Comment comment
  <?> "Comment"

spaceLine :: CharParser st Char
spaceLine = satisfy (\c -> c /= '\n' && c /= '\r' && isSpace(c))

spacesLine :: TokenParser String
spacesLine = many spaceLine

spacesLine1 :: TokenParser String
spacesLine1 = many1 spaceLine

parseListElementsLine :: TokenParser [Token]
parseListElementsLine = do
  clusters <- sepEndBy parseElementCluster spacesLine1
  let catted = concat clusters
  return catted

-- TODO: Allow parsing of literate comment lines in explicit list blocks
parseListElements :: TokenParser [Token]
parseListElements = do
  clusters <- sepEndBy parseElementCluster spaces1
  let catted = concat clusters
  return catted

parseElementCluster :: TokenParser [Token]
parseElementCluster = parsePrefixInfix <|> parseFirstItem

parsePrefixInfix :: TokenParser [Token]
parsePrefixInfix = do
  op <- parseOperatorString 
  item <- option [] parseItemAsList
  case item of
    (_:[]) -> do
      more <- many parseInfixPostfix
      return $ PrefixOperator op : (item ++ (concat more))
    otherwise -> do 
      return $ [Operator op]

parseFirstItem :: TokenParser [Token]
parseFirstItem = do
  item <- parseItemAsList
  more <- many parseInfixPostfix
  return $ item ++ (concat more)

parseInfixPostfix :: TokenParser [Token]
parseInfixPostfix = do
  op <- parseOperatorString
  item <- option [] parseItemAsList
  return $ case item of
    (_:[]) -> Operator op : item
    otherwise -> [PostfixOperator op]

parseItem :: TokenParser Token
parseItem = parseComment
      <|> parseQuoted 
      <|> parseDoubleQuoted 
      <|> parseNumber
      <|> parseSymbol
      <|> parseList
      <|> parseSquareBracketList
      <|> parseCurlyBraceList

parseList :: TokenParser Token
parseList = do
  begin <- char '('
  elements <- many parseListElements
  end <- char ')'
  return $ List $ elements

parseSquareBracketList :: TokenParser Token
parseSquareBracketList = do
  begin <- char '['
  elements <- many parseListElements
  end <- char ']'
  return $ SquareBracketList $ elements

parseCurlyBraceList :: TokenParser Token
parseCurlyBraceList = do
  begin <- char '{'
  elements <- many parseListElements
  end <- char '}'
  return $ CurlyBraceList $ elements

parseItemAsList :: TokenParser [Token]
parseItemAsList = do
  item <- parseItem
  return [item]

-- Numeric literals

parseNumber :: TokenParser Token
parseNumber = do 
        -- parse a 0 then the number
        char '0'
        num <- zeroNumFloat
        return num
    -- or parse a decimal/float
    <|> decimalFloat ""
  <?> "Number"
                  
zeroNumFloat :: TokenParser Token
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
  
decimalFloat :: String -> TokenParser Token
decimalFloat zero = do 
  -- try to parse a decimal
  num <- decimal
  let znum = zero ++ num
  -- see if there's a trailing fractional component
  option (Integer num) (fractFloat znum)

fractFloat :: String -> TokenParser Token
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

exponent' :: TokenParser String
exponent' = do
    e <- oneOf "eE"
    s <- sign
    exp <- decimal <?> "exponent"
    return $ e : s ++ exp
  <?> "exponent"

sign :: TokenParser String
sign = option ("") (string "-" <|> string "+") <?> "Exponent Sign"

decimal :: TokenParser String
decimal = many1 digit 

hexadecimal :: TokenParser Token
hexadecimal = do
    x <- oneOf "xX"
    hex <- many1 hexDigit
    return $ Hexadecimal ('0' : x : hex)
  <?> "Hexadecimal Integer"

octal :: TokenParser Token
octal = do 
    o <- oneOf "oO"
    oct <- many1 octDigit
    return $ Octal ('0' : o : oct)
  <?> "Octal Integer"

binary :: TokenParser Token
binary = do 
    b <- oneOf "bB"
    bin <- many1 binaryDigit
    return $ Binary ('0' : b : bin)
  <?> "Binary Integer"

-- parse a binary digit
binaryDigit :: TokenParser Char
binaryDigit = satisfy (\c -> c == '0' || c == '1') <?> "binary digit"


-- Parser Functions

-- TODO: Review this code
parseModule :: Parser Element
parseModule = do
  elements <- many parseElement
  return $ List elements

parseElement :: Parser Element
parseElement = parseIndentedList <|> parseItemElement

parseItemElement :: Parser Element
parseItemElement = tokenEquals Element

parseIndentedList :: Parser Element
parseIndentedList = do
    tokenEquals $ Begin delim
    x <- manyTill parseElement (tokenEquals $ End delim)
    tokenEquals $ End delim
    return $ List delim x
  <?> "Indented List"

