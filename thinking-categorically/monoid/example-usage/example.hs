import Data.Monoid

-- Using Sum monoid for addition
example1 :: Int
example1 = getSum $ mconcat [Sum 1, Sum 2, Sum 3, Sum 4]
-- Result: 10

-- Using Product monoid for multiplication
example2 :: Int
example2 = getProduct $ mconcat [Product 2, Product 3, Product 4]
-- Result: 24

-- Working with strings (concatenation)
example3 :: String
example3 = mconcat ["Hello", " ", "World", "!"]
-- Result: "Hello World!"

-- Folding with empty case handling
safeSum :: [Int] -> Int
safeSum xs = getSum $ foldMap Sum xs
-- safeSum [] = 0 (identity element)
-- safeSum [1,2,3] = 6

-- Custom monoid for tracking statistics
data Stats = Stats { count :: Int, total :: Int } deriving (Show)

instance Semigroup Stats where
  Stats c1 t1 <> Stats c2 t2 = Stats (c1 + c2) (t1 + t2)

instance Monoid Stats where
  mempty = Stats 0 0

-- Usage: combining statistics from multiple sources
combineStats :: [Int] -> Stats
combineStats = foldMap (\x -> Stats 1 x)
-- combineStats [10, 20, 30] = Stats {count = 3, total = 60}
-- combineStats [] = Stats {count = 0, total = 0}

main :: IO ()
main = do
  print example1
  print example2
  print example3

  print $ safeSum [1,2,3]
  print $ safeSum []

  print $ combineStats [1, 2, 3]
