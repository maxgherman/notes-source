import Prelude hiding ((.))

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

f :: Int -> String
f x = show x

g :: String -> Bool
g s = length s > 2

h :: Int -> Bool
h = g . f  -- Composition

main = print (h 123)  -- Output: True
