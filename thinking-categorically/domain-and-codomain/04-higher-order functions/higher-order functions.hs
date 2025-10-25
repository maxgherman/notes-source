import Language.Haskell.TH (prim)
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

doubled :: [Int]
doubled = map (*2) numbers        -- [2, 4, 6, 8, 10]

evens :: [Int]
evens = filter even numbers       -- [2, 4]

-- add returns a function
add :: Int -> (Int -> Int)
add x = \y -> x + y

-- Equivalent using currying
add' :: Int -> Int -> Int
add' x y = x + y

-- Both create functions:
addFive :: Int -> Int
addFive = add 5

result1 :: Int
result1 = addFive 3  -- 8

-- Domain analysis:
-- add has domain: Int
-- add has codomain: (Int -> Int) - a function type!

-- Function composition
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--        ^^^^^^^^^    ^^^^^^^    ^^^^^^^
--        function     function   resulting
--         g: b->c      f: a->b    function

-- compose takes two functions and returns their composition
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = \x -> g (f x)

-- Example
toString :: Int -> String
toString = show

doubleIt :: Int -> Int
doubleIt = (*2)

-- Compose them
doubleAndShow :: Int -> String
doubleAndShow = compose toString doubleIt
-- Or using the operator: doubleAndShow = toString . doubleIt

result2 = doubleAndShow 21  -- "42"

main :: IO ()
main = do
    print result1
    print result2