{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Category
import Prelude hiding ((.), id, curry, uncurry)
import Data.Kind (Type)

-- Cartesian structure (products)
class Category k => Cartesian k where
  -- Injective associated type: the product object uniquely determines its factors
  type Product k a b = p | p -> a b
  exl   :: k (Product k a b) a
  exr   :: k (Product k a b) b
  (&&&) :: k x a -> k x b -> k x (Product k a b)
  infixr 3 &&&

-- Exponentials / CCC structure
class Cartesian k => CCC k where
  type Exp k :: Type -> Type -> Type
  apply   :: k (Product k (Exp k a b) a) b                 -- eval
  curryC  :: k (Product k a b) c -> k a (Exp k b c)         -- curry
  uncurryC :: k a (Exp k b c) -> k (Product k a b) c        -- uncurry

-- Instance for ordinary functions ----------------------------------
instance Cartesian (->) where
  type Product (->) a b = (a, b)
  exl = fst
  exr = snd
  (f &&& g) x = (f x, g x)

instance CCC (->) where
  type Exp (->) = (->)
  apply (f, a) = f a
  curryC f a b = f (a, b)
  uncurryC f (a, b) = f a b

-- Example usage with the standard interface ------------------------
libraryExample :: IO ()
libraryExample = do
  putStrLn "=== Using Simple CCC Interface ==="
  let combine :: (Int, String) -> String
      combine (n, s) = s ++ " " ++ show n
  let curriedCombine = curryC combine           -- Int -> String -> String
  let testFunc = curriedCombine 42              -- String -> String
  let result = apply (testFunc, "Answer:")
  putStrLn $ "apply (curryC combine 42, \"Answer:\") = " ++ result
  let uncurriedBack = uncurryC curriedCombine   -- (Int,String) -> String
  putStrLn $ "uncurryC (curryC combine) (100, \"Count:\") = " ++ uncurriedBack (100, "Count:")
  -- Demonstrate product projections & pairing with explicit types to avoid defaulting warnings
  let samplePair :: (Int, String)
      samplePair = (10 :: Int, "ten")
  putStrLn $ "exl (10,\"ten\") = " ++ show (exl samplePair)
  putStrLn $ "exr (10,\"ten\") = " ++ show (exr samplePair)
  let paired :: (Int, String)
      paired = ((+1) &&& show) (7 :: Int)
  putStrLn $ "((+1) &&& show) 7 = " ++ show paired

-- Abstract composition using ordinary categorical composition ------
abstractCCCLibrary :: (CCC k) => (a `k` b) -> (b `k` c) -> (a `k` c)
abstractCCCLibrary f g = g . f

-- Advantages --------------------------------------------------------
exampleAdvantages :: IO ()
exampleAdvantages = do
  putStrLn "\n=== CCC Advantages ==="
  putStrLn "• Functions as exponentials (internal homs)"
  putStrLn "• Products + exponentials model simply typed lambda calculus"
  putStrLn "• Currying / uncurrying witness the adjunction"
  -- Simple arithmetic example
  let simple = curryC (\(x,y) -> x + y) :: Int -> Int -> Int
  putStrLn $ "curryC (\\(x,y)->x+y) 3 4 = " ++ show (simple 3 4)

main :: IO ()
main = do
  libraryExample
  exampleAdvantages
  putStrLn "\nComposition example: (show . (+1)) 5 ="
  let comp :: Int -> String
      comp = abstractCCCLibrary (+1) show
  putStrLn (comp 5)
