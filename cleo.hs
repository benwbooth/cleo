module Main where
import IO hiding(try)
import Monad
import Data.Char (isSymbol, isPunctuation, isSpace)
import Data.List (concat, isPrefixOf, findIndex)
import System.Console.GetOpt

--import Test.QuickCheck
import Text.ParserCombinators.Parsec hiding (Parser)
import System.Environment.UTF8
--import System.Console.Readline
--import Data.Decimal
--import LLVM.Core
--import LLVM.ExecutionEngine

-- tokens
data Item = Symbol String
  | InfixOperator String
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
  | PostComment String
  deriving Show

-- block delimiter types
data Delim = Parens
  | Square
  | Curly
  | Indent
  deriving Show

-- tokens + indented delimiters
type Token = (SourcePos, Tok)
data Tok = Token Item
  | Begin Delim
  | End Delim
  deriving Show

-- parsed elements
data Element = Element Item
  | List [Element]
  | SquareBracketList [Element]
  | CurlyBraceList [Element]
  deriving Show

type Parser a = GenParser Char () a
type IndentParser a = GenParser Token () a

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

-- * Indented list parsing

parseTokens :: Parser [Token]
parseTokens = parseLines [] [""] 

parseLines :: [Token] -> [String] -> Parser [Token]
parseLines tokens indentStack = do
  option [] (parseLiterateComment tokens indentStack
            <|> parseTokenLines tokens indentStack)
  return tokens

-- Unindented lines are literate comments
parseLiterateComment :: [Token] -> [String] -> Parser [Token]
parseLiterateComment tokens indentStack = do
    lines <- many1 parseLiterateCommentLine
    parseLines (tokens ++ [Token $ Comment (concat lines)]) indentStack
  <?> "Literate comment"

parseLiterateCommentLine :: Parser String
parseLiterateCommentLine = do
      -- make sure this line is unindented
      notFollowedBy spaceLine
      line <- many1 (notFollowedBy eol >> anyChar)
      eol_ <- eol
      return $ line ++ eol_
    <?> "Literate comment line"

parseTokenLines :: [Token] -> [String] -> Parser [Token]
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

-- whitespace parsing

eol :: Parser String
eol = string "\n" 
    <|> do
      a <- string "\r"
      b <- option "" (string "\n")
      return $ a ++ b
  <?> "eol"

spaces1 :: Parser ()
spaces1 = skipMany1 space

spaceLine :: CharParser st Char
spaceLine = satisfy (\c -> c /= '\n' && c /= '\r' && isSpace(c))

spacesLine :: Parser String
spacesLine = many spaceLine

spacesLine1 :: Parser String
spacesLine1 = many1 spaceLine

parseSymbol :: Parser [Token]
parseSymbol = do
    first <- letter <|> char '_'
    rest <- many (letter <|> digit <|> char '_')
    let symbol = first : rest
    return [Token $ Symbol symbol]
  <?> "Symbol"

parseIndentedString :: Parser [Token]
parseIndentedString = do
  preComments <- many parseNestedLiterateComment
  indent <- spacesLine1
  str <- many (notFollowedBy eol >> anyChar)
  eol_ <- eol
  rest <- many (parseIndentedStringLine indent)
  return $ (concat preComments) ++ [Token $ String (str++eol_)] ++ (concat rest)

parseIndentedStringLine :: String -> Parser [Token]
parseIndentedStringLine indent = try $ do
  string indent
  str <- many (notFollowedBy eol >> anyChar)
  eol_ <- eol
  postComments <- many parseNestedLiterateComment
  return $ (Token (String (str++eol_))) : (concat postComments)

parseQuoted :: Parser [Token]
parseQuoted = do
    char '\''
    string <- (((try (string "''" >> eol)) >> parseIndentedString)
      <|> do
          string <- many $ (notFollowedBy (string "'" <|> eol) >> anyChar) <|> try (string "''" >> return '\'')
          char '\''
          return [Token $ String string])
    return string

parseDoubleQuoted :: Parser [Token]
parseDoubleQuoted = do
    char '"'
    string <- (((try (string "''" >> eol)) >> parseIndentedString)
      <|> do
          string <- many $ (notFollowedBy (string "\"" <|> eol) >> anyChar) <|> try (string "\"\"" >> return '"')
          char '"'
          return [Token $ String string])
    return string

parseBacktickQuoted :: Parser String
parseBacktickQuoted = do
    char '`'
    x <- many $ noneOf "`\n" <|> try (string "``" >> return '`')
    char '`'
    return x
  <?> "Backquoted Operator"

parseOperatorString :: Parser String
parseOperatorString = do 
    operator <- parseBacktickQuoted 
      <|> (many1 (satisfy (\c -> 
          c /= '(' && c /= ')' && 
          c /= '{' && c /= '}' && 
          c /= '[' && c /= ']' && 
          c /= '\'' && c /= '"' && c /= '`' &&
          c /= '_' && 
          (isSymbol c || isPunctuation c))) <?> "InfixOperator")
    return operator
  <?> "InfixOperator"

parseCommentString :: Parser String
parseCommentString = do
    try $ string "\\\\"
    comment <- many ((notFollowedBy eol) >> anyChar)
    eol_ <- eol
    return $ comment ++ eol_
  <?> "Comment"

parseComment :: Parser [Token]
parseComment = do
  comment <- parseCommentString
  return $ [Token $ Comment comment]

parsePostComment :: Parser [Token]
parsePostComment = do
  comment <- parseCommentString
  return $ [Token (PostComment comment)]

-- parse a single line of elements
parseListElementsLine :: Parser [Token]
parseListElementsLine = do
  parseComment
      <|> do
          elements <- sepEndBy parseElementCluster spacesLine1
          postComment <- option [] parsePostComment
          return $ (concat elements) ++ postComment

parseElementCluster :: Parser [Token]
parseElementCluster = parsePrefixInfix <|> parseFirstItem

parsePrefixInfix :: Parser [Token]
parsePrefixInfix = do
  op <- parseOperatorString 
  item <- option [] parseItem
  case item of
    (_:[]) -> do
      more <- many parseInfixPostfix
      return $ (Token $ PrefixOperator op) : (item ++ (concat more))
    otherwise -> do 
      return $ [Token $ InfixOperator op]

parseFirstItem :: Parser [Token]
parseFirstItem = do
  item <- parseItem
  more <- many parseInfixPostfix
  return $ item ++ (concat more)

parseInfixPostfix :: Parser [Token]
parseInfixPostfix = do
  op <- parseOperatorString
  item <- option [] parseItem
  return $ case item of
    (_:[]) -> (Token $ InfixOperator op) : item
    otherwise -> [Token $ PostfixOperator op]

parseItem :: Parser [Token]
parseItem = do
  item <- (parseQuoted 
        <|> parseDoubleQuoted 
        <|> parseNumber
        <|> parseSymbol
        <|> parseList
        <|> parseSquareBracketList
        <|> parseCurlyBraceList)
  return item

parseNestedLiterateComment :: Parser [Token]
parseNestedLiterateComment = do
    lines <- many1 parseLiterateCommentLine
    return [Token $ Comment (concat lines)]
  <?> "Literate comment"

parseNestedTokenLine :: Parser [Token]
parseNestedTokenLine = do
  indent <- spacesLine1
  elements <- parseListElementsLine
  return elements

parseList :: Parser [Token]
parseList = do
  begin <- char '('
  elements <- many (parseNestedLiterateComment
                <|> parseNestedTokenLine)
  end <- char ')'
  return $ concat elements

parseSquareBracketList :: Parser [Token]
parseSquareBracketList = do
  begin <- char '['
  elements <- many (parseNestedLiterateComment
                <|> parseNestedTokenLine)
  end <- char ']'
  return $ concat elements

parseCurlyBraceList :: Parser [Token]
parseCurlyBraceList = do
  begin <- char '{'
  elements <- many (parseNestedLiterateComment
                <|> parseNestedTokenLine)
  end <- char '}'
  return $ concat elements

-- Tokenizer Functions
--
satisfyToken :: (Token -> Maybe a) -> Parser a
satisfyToken test 
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

tokenEquals :: Parser a
tokenEquals = satisfyToken (==)

-- IndentParser Functions

parseModule :: IndentParser Element
parseModule = do
  elements <- many parseElement
  return $ List elements

parseElement :: IndentParser Element
parseElement = parseIndentedList <|> parseItemElement

parseItemElement :: IndentParser Element
parseItemElement = tokenEquals Element

parseIndentedList :: IndentParser Element
parseIndentedList = do
    tokenEquals $ Begin Indent
    elements <- manyTill parseElement (tokenEquals $ End Indent)
    return $ List elements
  <?> "Indented List"

-- Numeric literals

parseNumber :: Parser [Token]
parseNumber = do 
        -- parse a 0 then the number
        char '0'
        num <- zeroNumFloat
        return [num]
    -- or parse a decimal/float
    <|> decimalFloat ""
  <?> "Number"
                  
zeroNumFloat :: Parser Token
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
  <|> return [Token $ Integer "0"]
  
decimalFloat :: String -> Parser Token
decimalFloat zero = do 
  -- try to parse a decimal
  num <- decimal
  let znum = zero ++ num
  -- see if there's a trailing fractional component
  option (Token $ Integer num) (fractFloat znum)

fractFloat :: String -> Parser Token
fractFloat num = do 
    -- parse the fractional part
    char '.'
    fract <- many1 digit
    exp <- option "" exponent'
    return $ Token $ Float $ num ++ "." ++ fract ++ exp
  <|>
  -- just parse the exponent part (e.g. 0e-10)
  do exp <- exponent'
     return $ Token $ Float (num ++ exp)

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

hexadecimal :: Parser Token
hexadecimal = do
    x <- oneOf "xX"
    hex <- many1 hexDigit
    return $ Token $ Hexadecimal ('0' : x : hex)
  <?> "Hexadecimal Integer"

octal :: Parser Token
octal = do 
    o <- oneOf "oO"
    oct <- many1 octDigit
    return $ Token $ Octal ('0' : o : oct)
  <?> "Octal Integer"

binary :: Parser Token
binary = do 
    b <- oneOf "bB"
    bin <- many1 binaryDigit
    return $ Token $ Binary ('0' : b : bin)
  <?> "Binary Integer"

-- parse a binary digit
binaryDigit :: Parser Char
binaryDigit = satisfy (\c -> c == '0' || c == '1') <?> "binary digit"


