-- Natural transformation from List to Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Verification of naturality:
-- For any function f :: a -> b and list xs :: [a]:
-- fmap f (safeHead xs) == safeHead (fmap f xs)

-- Example:
example2 :: Bool
example2 = let f = show  -- Int -> String
               xs = [1, 2, 3] :: [Int]
           in fmap f (safeHead xs) == safeHead (map f xs)  -- True

main :: IO ()
main = print example2