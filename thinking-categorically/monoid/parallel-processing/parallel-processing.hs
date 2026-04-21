{-# LANGUAGE ParallelListComp #-}
import Control.Parallel.Strategies
import Data.Monoid (Sum(..), getSum)

-- Simple parallel fold using monoid properties
parallelFold :: Monoid a => [a] -> a
parallelFold [] = mempty  -- Identity element handles empty case automatically!
parallelFold xs =
  let chunkSize = max 1 (length xs `div` 4)  -- Divide among 4 cores
      chunks = chunksOf chunkSize xs
      -- Each chunk folds to its local result, empty chunks become mempty
      chunkResults = map (foldMap id) chunks `using` parList rseq
  in foldMap id chunkResults  -- Combine all results

-- Enhanced parallel map-fold with guaranteed safety
parallelMapFold :: Monoid b => (a -> b) -> [a] -> b
parallelMapFold f xs =
  let chunks = chunksOf 1000 xs
      -- Each worker processes its chunk independently
      chunkResults = map (foldMap f) chunks `using` parList rseq
  in foldMap id chunkResults  -- Always safe, even with empty chunks

-- Parallel statistics collection using multiple monoids
data Stats = Stats { count :: Sum Int, total :: Sum Int, squares :: Sum Int }
  deriving (Show, Eq)

instance Semigroup Stats where
  Stats c1 t1 s1 <> Stats c2 t2 s2 = Stats (c1 <> c2) (t1 <> t2) (s1 <> s2)

instance Monoid Stats where
  mempty = Stats mempty mempty mempty

-- Collect statistics in parallel
parallelStats :: [Int] -> Stats
parallelStats = parallelMapFold (\x -> Stats (Sum 1) (Sum x) (Sum (x * x)))

-- Helper function
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

-- Example usage
main :: IO ()
main = do
  let numbers = [1..1000000] :: [Int]  -- Explicit type annotation
  let emptyList = [] :: [Int]

  -- Direct parallel fold of monoid values
  let sums = map Sum ([1..100] :: [Int])  -- Explicit type for the range
  let foldResult = parallelFold sums
  putStrLn $ "Parallel fold result: " ++ show (getSum foldResult)

  -- Safe parallel sum - works even with empty lists!
  let sumResult = parallelMapFold Sum numbers
  let emptySumResult = parallelMapFold Sum emptyList
  putStrLn $ "Parallel sum: " ++ show (getSum sumResult)
  putStrLn $ "Empty list sum: " ++ show (getSum emptySumResult)  -- 0, no Maybe needed!

  -- Parallel statistics
  let stats = parallelStats [1..100]
  putStrLn $ "Parallel stats: " ++ show stats

  -- Empty stats are safe too
  let emptyStats = parallelStats []
  putStrLn $ "Empty stats: " ++ show emptyStats  -- All zeros, perfectly safe
