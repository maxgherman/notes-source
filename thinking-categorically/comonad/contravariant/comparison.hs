import Data.Functor.Contravariant (Contravariant (contramap))

-- Comparison is contravariant in both arguments
newtype Comparison a = Comparison (a -> a -> Ordering)

runComparison :: Comparison a -> a -> a -> Ordering
runComparison (Comparison cmp) = cmp

instance Contravariant Comparison where
  contramap f (Comparison cmp) = Comparison (\x y -> cmp (f x) (f y))

-- Usage
intCompare :: Comparison Int
intCompare = Comparison compare

-- Reuse for any type that has a length
lengthCompare :: Comparison String
lengthCompare = contramap length intCompare

main :: IO ()
main = do
  print $ runComparison intCompare 10 20
  print $ runComparison lengthCompare "cat" "elephant"
  print $ runComparison lengthCompare "same" "size"
