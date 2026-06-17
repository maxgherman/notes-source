{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import Data.Char (isDigit, isAlpha, isSpace)

-- Parser with MonadPlus
newtype Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    [(f a, rest) | (a, rest) <- p input]

instance Applicative Parser where
  pure a = Parser $ \input -> [(a, input)]
  Parser pf <*> Parser pa = Parser $ \input ->
    [(f a, rest2) | (f, rest1) <- pf input, (a, rest2) <- pa rest1]

instance Alternative Parser where
  empty = Parser $ \_ -> []
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      [] -> p2 input      -- Only try second if first fails completely
      result -> result    -- Use first parser's result if it succeeds

instance Monad Parser where
  Parser p >>= f = Parser $ \input ->
    [(b, rest2) | (a, rest1) <- p input, (b, rest2) <- runParser (f a) rest1]

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- Basic parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    (x:xs) | predicate x -> [(x, xs)]
    _ -> []

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c:cs)

-- Whitespace handling
spaces :: Parser ()
spaces = void $ many $ satisfy isSpace

lexeme :: Parser a -> Parser a
lexeme p = do
  result <- p
  spaces
  return result

-- Number parsing with different formats
decimal :: Parser Int
decimal = read <$> some (satisfy isDigit)

hexadecimal :: Parser Int
hexadecimal = do
  _ <- string "0x" <|> string "0X"
  digits <- some (satisfy (\c -> isDigit c || c `elem` ("abcdefABCDEF" :: String)))
  return $ read ("0x" ++ digits)

binary :: Parser Int
binary = do
  _ <- string "0b" <|> string "0B"
  digits <- some (satisfy (`elem` ("01" :: String)))
  return $ foldl (\acc d -> acc * 2 + read [d]) 0 digits

-- MonadPlus: try different number formats
number :: Parser Int
number = lexeme $ hexadecimal <|> binary <|> decimal

-- Expression parsing with backtracking
data Expr = Num Int | Add Expr Expr | Mul Expr Expr | Var String
  deriving (Show, Eq)

variable :: Parser String
variable = lexeme $ do
  first <- satisfy isAlpha
  rest <- many (satisfy (\c -> isAlpha c || isDigit c))
  return (first:rest)

factor :: Parser Expr
factor = (Num <$> number) <|> (Var <$> variable) <|> parens expr

parens :: Parser a -> Parser a
parens p = do
  _ <- lexeme $ char '('
  result <- p
  _ <- lexeme $ char ')'
  return result

term :: Parser Expr
term = chainl1 factor (Mul <$ lexeme (char '*'))

expr :: Parser Expr
expr = chainl1 term (Add <$ lexeme (char '+'))

-- Helper for left-associative operators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y)) <|> return x

-- Parse with multiple strategies and error recovery
parseExpression :: String -> Either String Expr
parseExpression input =
  case runParser (spaces >> expr) input of
    [(result, "")] -> Right result
    [(_, remaining)] -> Left $ "Unexpected input: " ++ remaining
    [] -> Left "Parse error: no valid parse"
    _ -> Left "Parse error: ambiguous parse"

-- Usage examples
main :: IO ()
main = do
  print $ parseExpression "42"                   -- Right (Num 42)
  print $ parseExpression "0xFF"                 -- Right (Num 255)
  print $ parseExpression "0b1010"               -- Right (Num 10)
  print $ parseExpression "x + 42"               -- Right (Add (Var "x") (Num 42))
  print $ parseExpression "(a + 0x10) * b"       -- Right (Mul (Add (Var "a") (Num 16)) (Var "b"))
  print $ parseExpression "invalid@"             -- Left "Unexpected input: @"
