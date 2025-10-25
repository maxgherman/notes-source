-- This compiles because types align
validComposition :: String -> Int
validComposition = length . words
--                ^      ^
--                |      domain: String, codomain: [String]
--                domain: [String], codomain: Int

-- This does NOT compile due to type mismatch
-- invalidComposition = show . words
--                     ^    ^
--                     |    domain: String, codomain: [String]
--                     domain: Int, codomain: String
--                     MISMATCH: [String] ≠ Int


main :: IO ()
main = print (validComposition "123")