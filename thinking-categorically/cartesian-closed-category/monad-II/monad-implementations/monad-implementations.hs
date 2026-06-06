{-# LANGUAGE RebindableSyntax #-}

module Main where

import Prelude hiding (Either(..), Maybe(..), Monad(..))

-- The Monad typeclass definition
class Applicative m => Monad m where
    return :: a -> m a           -- Unit operation (lift value into monad)
    (>>=)  :: m a -> (a -> m b) -> m b  -- Bind operation (chain computations)

    -- Default implementation of >> (sequence operator)
    (>>) :: m a -> m b -> m b
    m1 >> m2 = m1 >>= \_ -> m2

-- Maybe Monad Implementation
data Maybe a = Nothing | Just a deriving (Show, Eq)

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

instance Monad Maybe where
    return = Just
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x

-- Either Monad Implementation
data Either a b = Left a | Right b deriving (Show, Eq)

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either a) where
    pure = Right
    Left e <*> _ = Left e
    Right f <*> r = fmap f r

instance Monad (Either a) where
    return = Right
    Left e  >>= _ = Left e
    Right v >>= f = f v

-- List Monad Implementation
instance Monad [] where
    return x = [x]
    xs >>= f = concatMap f xs  -- Apply f to each element and flatten

-- IO Monad (built into Haskell runtime)
-- instance Monad IO where
--     return = primitive operation
--     (>>=) = primitive operation

-- Example usage
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

calculation :: Maybe Double
calculation = do
    x <- Just 10
    y <- Just 2
    z <- safeDiv x y
    return (z * 3)
-- Result: Just 15.0

-- Using bind directly
calculation' :: Maybe Double
calculation' = Just 10 >>= \x ->
               Just 2  >>= \y ->
               safeDiv x y >>= \z ->
               return (z * 3)

main :: IO ()
main = print (calculation, calculation')
