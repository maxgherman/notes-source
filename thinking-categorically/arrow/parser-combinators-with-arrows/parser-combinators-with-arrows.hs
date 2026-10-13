import Control.Arrow
import Control.Category
import Data.Char (ord)
import Prelude hiding (id, (.))

-- Simple parser Arrow (simplified for demonstration)
newtype SimpleParser a b = SimpleParser { runSimpleParser :: a -> Maybe b }

instance Category SimpleParser where
  id = SimpleParser Just
  SimpleParser f . SimpleParser g = SimpleParser $ \input -> do
    intermediate <- g input
    f intermediate

instance Arrow SimpleParser where
  arr f = SimpleParser (Just . f)
  first (SimpleParser p) = SimpleParser $ \(a, c) -> do
    b <- p a
    return (b, c)

-- Basic parsing primitives for characters
charParser :: Char -> SimpleParser String Char
charParser target = SimpleParser $ \input -> case input of
  []     -> Nothing
  (c:_) -> if c == target then Just c else Nothing

digitParser :: SimpleParser Char Int
digitParser = SimpleParser $ \c ->
  if c >= '0' && c <= '9'
  then Just (ord c - ord '0')
  else Nothing

-- Arrow-based parser combinators
parseSpecificDigit :: Char -> SimpleParser String Int
parseSpecificDigit d = charParser d >>> digitParser

-- Simple number parser (parses first character only for demo)
parseFirstDigit :: SimpleParser String Int
parseFirstDigit = SimpleParser $ \input -> case input of
  []    -> Nothing
  (c:_) -> if c >= '0' && c <= '9'
           then Just (ord c - ord '0')
           else Nothing

-- Working example functions
main :: IO ()
main = do
  putStrLn "Testing parseFirstDigit:"
  print $ runSimpleParser parseFirstDigit "5abc"
  putStrLn "Testing charParser:"
  print $ runSimpleParser (charParser 'a') "abc"
  putStrLn "Testing parseSpecificDigit:"
  print $ runSimpleParser (parseSpecificDigit '7') "7xyz"

-- Example usage:
-- runSimpleParser parseFirstDigit "123abc" returns Just 1
-- runSimpleParser (charParser 'a') "abc" returns Just 'a'
