add1 :: Int -> Int
add1 x = x + 1

-- Apply using applicative style
example1 = pure add1 <*> pure 2   -- Just 3

-- Apply directly
example2 = pure (add1 2)          -- Just 3

example1 == example2
