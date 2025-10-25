-- Function signature shows domain and codomain
length :: [a] -> Int
--       ^      ^
--       |      codomain (Int)
--       domain ([a] - list of any type)

show :: Int -> String
--     ^      ^
--     |      codomain (String)
--     domain (Int)

-- Composition requires matching types
showLength :: [a] -> String
showLength = show . length
--          ^   ^
--          |   domain of length: [a]
--          codomain of length: Int
--          domain of show: Int

-- Example usage
result = showLength [1,2,3,4]  -- "4"