-- Natural transformation from [[a]] to [a]
flatten :: [[a]] -> [a]
flatten = concat

-- Verification of naturality:
-- For any function f :: a -> b and nested list xss :: [[a]]:
-- map f (flatten xss) == flatten (map (map f) xss)

example3 :: Bool
example3 = let f = (*2) :: Int -> Int
               xss = [[1, 2], [3, 4]] :: [[Int]]
           in map f (flatten xss) == flatten (map (map f) xss)  -- True

main :: IO ()
main = print example3