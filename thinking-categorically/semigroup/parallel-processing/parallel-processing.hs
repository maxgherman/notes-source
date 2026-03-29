import Control.DeepSeq (NFData)
import Control.Parallel.Strategies
import Data.List (foldl1')
import Data.Semigroup (Sum(..), getSum)

-- Parallel divide-and-conquer combiner
parallelCombine :: (NFData a, Semigroup a) => [a] -> Maybe a
parallelCombine [] = Nothing
parallelCombine [x] = Just x
parallelCombine xs =
  let mid = length xs `div` 2
      (left, right) = splitAt mid xs
      [leftResult, rightResult] =
        map parallelCombine [left, right] `using` parList rdeepseq
  in case (leftResult, rightResult) of
       (Just l, Just r) -> Just (l <> r)
       (Just l, Nothing) -> Just l
       (Nothing, Just r) -> Just r
       (Nothing, Nothing) -> Nothing

-- Parallel map-reduce using strategies
parallelMapReduce :: (NFData b, Semigroup b) => (a -> b) -> [a] -> Maybe b
parallelMapReduce f xs =
  let mapped = map f xs `using` parList rdeepseq
      chunks = chunksOf 1000 mapped  -- Process in chunks
      chunkResults = map (foldl1' (<>)) chunks `using` parList rdeepseq
  in case chunkResults of
       [] -> Nothing
       _  -> Just $ foldl1' (<>) chunkResults

-- Helper function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

-- Example usage
main :: IO ()
main = do
  let numbers = [1..1000000]
  let smallNumbers = [1..100]

  -- Demonstrate parallelCombine with smaller dataset
  let wrappedSmallNumbers = map (Sum . fromIntegral :: Int -> Sum Integer) smallNumbers
  let combineResult = parallelCombine wrappedSmallNumbers
  putStrLn $ "Parallel combine result: " ++ show (getSum <$> combineResult)

  -- Parallel sum calculation using built-in Sum semigroup
  let sumResult = parallelMapReduce (Sum . fromIntegral :: Int -> Sum Integer) numbers
  putStrLn $ "Parallel map-reduce result: " ++ show (getSum <$> sumResult)

  -- Compare with sequential
  let sequentialSum = sum numbers
  putStrLn $ "Sequential sum: " ++ show sequentialSum

  -- Show both approaches give same result for smaller dataset
  let sequentialSmall = sum smallNumbers
  putStrLn $ "Sequential small sum: " ++ show sequentialSmall
