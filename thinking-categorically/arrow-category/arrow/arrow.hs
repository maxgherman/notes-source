module Main (main) where

-- Define the sets as lists
aSet, bSet, cSet, dSet :: [Integer]
aSet = [1, 2]
bSet = [10, 20]
cSet = [3, 4]
dSet = [30, 40]

-- Define the functions
f :: Integer -> Integer
f 1 = 10
f 2 = 20
f _ = error "f: input not in A"

g :: Integer -> Integer
g 3 = 30
g 4 = 40
g _ = error "g: input not in C"

s :: Integer -> Integer
s 1 = 3
s 2 = 4
s _ = error "s: input not in A"

t :: Integer -> Integer
t 10 = 30
t 20 = 40
t _ = error "t: input not in B"

mapsInto :: Eq b => (a -> b) -> [a] -> [b] -> Bool
mapsInto fn domain codomain = all (`elem` codomain) (map fn domain)

-- Check commutativity for all elements in A
commutes :: Bool
commutes =
  and
    [ mapsInto f aSet bSet
    , mapsInto s aSet cSet
    , mapsInto g cSet dSet
    , mapsInto t bSet dSet
    , all (\a -> t (f a) == g (s a)) aSet
    ]

main :: IO ()
main = print commutes