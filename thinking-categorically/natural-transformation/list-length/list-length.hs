-- The List functor
-- fmap :: (a -> b) -> [a] -> [b]

-- Natural transformation from List to Const Int
listLength :: [a] -> Int
listLength = length

-- Verification of naturality:
-- For any function f :: a -> b and list xs :: [a]:
-- listLength (fmap f xs) == listLength xs
-- This holds because length doesn't depend on the elements

-- Example usage:
example1 :: Bool
example1 = let f = (*2) :: Int -> Int
               xs = [1, 2, 3] :: [Int]
           in listLength (map f xs) == listLength xs  -- True

main :: IO ()
main = print example1