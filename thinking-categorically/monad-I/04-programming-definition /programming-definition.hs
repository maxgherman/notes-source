module Main (main) where

-- Haskell: A type constructor that takes one type and produces another.
-- Maybe :: * -> *
-- Maybe Int wraps Int values that might be missing.
-- Maybe String wraps String values that might be missing.
--
-- Haskell: pure lifts values into the Maybe context:
-- pure 5       == Just 5
-- pure "hello" == Just "hello"

-- Problem: We have functions that return wrapped values
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

safeDivideBy :: Int -> Int -> Maybe Int
safeDivideBy divisor value = safeDivide value divisor

-- Chaining operations with bind
computation :: Maybe Int
computation =
    pure 20          -- Maybe Int
    >>= safeDivideBy 4 -- Maybe Int
    >>= safeDivideBy 2 -- Maybe Int
    -- Result: Just 2

-- Without monads, we'd need nested pattern matching:
-- case safeDivide 20 4 of
--   Nothing -> Nothing
--   Just x -> case safeDivide x 2 of
--     Nothing -> Nothing
--     Just y -> Just y

main :: IO ()
main = print computation
