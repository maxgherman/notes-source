example1 :: Maybe Int
example1 = pure id <*> Just 42
-- Result: Just 42

example2 :: [Int]
example2 = pure id <*> [1, 2, 3]
-- Result: [1, 2, 3]

