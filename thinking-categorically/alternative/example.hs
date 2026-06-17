{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Char (isDigit, isAlpha)

-- Simple parser type
newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  Parser pf <*> Parser pa = Parser $ \input ->
    case pf input of
      Nothing -> Nothing
      Just (f, rest1) -> case pa rest1 of
        Nothing -> Nothing
        Just (a, rest2) -> Just (f a, rest2)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      result -> result

-- Basic parsers
digit :: Parser Char
digit = Parser $ \input ->
  case input of
    (x:xs) | isDigit x -> Just (x, xs)
    _ -> Nothing

letter :: Parser Char
letter = Parser $ \input ->
  case input of
    (x:xs) | isAlpha x -> Just (x, xs)
    _ -> Nothing

-- Complex parser using Alternative
identifier :: Parser String
identifier = (:) <$> letter <*> many (letter <|> digit)

number :: Parser Int
number = read <$> some digit

-- Parse different types of tokens
token :: Parser String
token = identifier <|> (show <$> number) <|> string "(" <|> string ")"
  where
    string s = Parser $ \input ->
      if take (length s) input == s
        then Just (s, drop (length s) input)
        else Nothing

-- Usage example
parseExpression :: String -> Maybe [String]
parseExpression input = case runParser (many token) input of
  Just (tokens, "") -> Just tokens
  _ -> Nothing
  where
    runParser (Parser p) = p

-- Test cases
main :: IO ()
main = do
  print $ parseExpression "hello123"     -- Just ["hello123"]
  print $ parseExpression "42"           -- Just ["42"]
  print $ parseExpression "func(42)"     -- Just ["func","(","42",")"]
  print $ parseExpression "invalid@"     -- Nothing
