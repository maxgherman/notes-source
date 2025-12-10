-- Hom(A, -) corresponds to (A -> _)
-- For fixed type A = Int, we have Hom(Int, -):

-- Hom(Int, String) - functions from Int to String
toString :: Int -> String
toString = show

-- Hom(Int, Bool) - functions from Int to Bool
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- The functor action: fmap for (A -> _)
-- If f :: b -> c, then fmap f :: (a -> b) -> (a -> c)
-- This is just function composition!

addOne :: Int -> Int
addOne = (+1)

addOneAndShow :: Int -> String
addOneAndShow = fmap show addOne  -- Equivalent to: show . addOne

-- Example usage
main :: IO ()
main = do
  print $ toString 42      -- "42"
  print $ isEven 4         -- True
  print $ addOneAndShow 5  -- "6"

  -- Another example: transform Int -> Int into Int -> [Int]
  let singleton x = [x]
  let addOneToList = fmap singleton addOne  -- Int -> [Int]
  print $ addOneToList 10  -- [11]
