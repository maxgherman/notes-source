import Data.Functor.Contravariant (Contravariant (contramap))

-- Predicate is contravariant in its input type
newtype Predicate a = Predicate
  { runPredicate :: a -> Bool
  }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)
  --        ↑ preprocess input before applying predicate

-- Usage
isEven :: Predicate Int
isEven = Predicate even

-- Contramap allows us to reuse predicates
isEvenLength :: Predicate String
isEvenLength = contramap length isEven
-- length :: String -> Int (forward)
-- contramap makes it work on Strings

matching :: Predicate a -> [a] -> [a]
matching predicate = filter (runPredicate predicate)

main :: IO ()
main = do
  putStrLn "Test the original Predicate Int:"
  print $ runPredicate isEven 4
  print $ runPredicate isEven 7

  putStrLn "\nReuse it as a Predicate String with contramap:"
  print $ runPredicate isEvenLength "Haskell"
  print $ runPredicate isEvenLength "code"

  putStrLn "\nFilter values with both predicates:"
  print $ matching isEven [1 .. 10]
  print $ matching isEvenLength ["a", "to", "cat", "code", "lambda"]
