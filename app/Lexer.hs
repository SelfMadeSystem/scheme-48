module Lexer (LispVal, parseExpr, readExpr) where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec hiding (spaces)
import Types
import Data.Functor

-- | Parses one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- | Parses one symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Parses a string character
--
-- If the character is a backslash, it will parse the next character
-- and return the corresponding escape character
--
-- If the following character is a `u`, it will parse the next 4 characters
-- and return the corresponding unicode character
parseStringChar :: Parser Char
parseStringChar = do
  c <- noneOf "\""
  if c == '\\'
    then do
      c' <- anyChar
      if c' == 'u'
        then do
          codePoint <- count 4 hexDigit
          return $ toEnum (read codePoint)
        else case lookup c' escapeChars of
          Just x -> return x
          Nothing -> fail "Invalid escape character"
    else
      if c == '"'
        then fail "Unexpected end of string"
        else return c
  where
    escapeChars =
      [ ('\\', '\\'),
        ('"', '"'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t')
      ]

-- | Parses a string. Starts and ends with a double quote and can contain any
-- character. Escape sequences are supported.
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many parseStringChar
  _ <- char '"'
  return $ String x

-- | Parses a character. Characters are represented by a #\ followed by a
-- character or the name of a character.
--
-- The following names are supported:
-- - space
-- - newline
-- - tab
-- - return
parseChar :: Parser LispVal
parseChar = do
  _ <- string "#\\"
  c <- try (choice (map string specialChars)) <|> (anyChar <&> (:[]))
  return $ Char $ fromMaybe (head c) (lookup c charMap)
  where
    charMap = [("space", ' '), ("newline", '\n'), ("tab", '\t'), ("return", '\r')]
    specialChars = map fst charMap

-- | Parses an atom. An atom is a letter or a symbol followed by zero or more
-- letters, digits or symbols. If the atom is `#t` or `#f`, it will be parsed
-- as a boolean value.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- | Parses a number. Number must start with a digit or a minus sign followed by
-- a digit.
--
-- TODO: Add support for radix prefixes (e.g. #b, #o, #d, #x), floating point
-- numbers and scientific notation.
--
-- e.g. 123, -123, 0, -0
parseNumber :: Parser LispVal
parseNumber = do
  sign <- option '+' (char '-')
  num <- many1 digit
  let number = if sign == '-' then negate (read num) else read num
  return $ Number number

-- | Parses the dot, space and the expression that follows it for a dotted list.
parseListDot :: Parser LispVal
parseListDot = do
  _ <- char '.'
  spaces
  parseExpr

-- | Parses either a list or a dotted list. A list is a sequence of expressions
-- separated by spaces and enclosed in parentheses. A dotted list is a list
-- followed by a dot and another expression.
parseList :: Parser LispVal
parseList = do
  h <- sepEndBy parseExpr spaces
  t <- optionMaybe $ try parseListDot
  return $ case t of
    Nothing -> List h
    Just x -> DottedList h x

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  try parseNumber
    <|> try parseString
    <|> try parseChar
    <|> parseAtom
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- parseList
      _ <- char ')'
      return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> Error $ Parser err
  Right val -> val
