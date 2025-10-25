-- Unsafe partial function (can crash)
head :: [a] -> a
head (x:_) = x
head []    = error "empty list"  -- Runtime error!

-- Safe total function using Maybe
safeHead :: [a] -> Maybe a
safeHead []     = Nothing  -- Explicit representation of "no value"
safeHead (x:_)  = Just x   -- Wrapped successful result

-- Domain: [a] (all lists)
-- Codomain: Maybe a (optional values)
-- Now it's total - every input has a defined output


main :: IO ()
main = print (safeHead "123")