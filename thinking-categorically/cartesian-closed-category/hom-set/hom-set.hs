{-# LANGUAGE RankNTypes #-}

-- The Hom-set Hom(A, B) is represented by the function type A -> B
type Hom a b = a -> b

-- In Haskell, A -> B IS the exponential object B^A
-- So Hom(A, B) ≅ B^A is literally true!

-- The currying isomorphism: Hom(A × B, C) ≅ Hom(A, C^B)
-- In Haskell types: ((A, B) -> C) ≅ (A -> (B -> C))

-- Higher-order functions work with Hom-sets as first-class objects
mapHom :: Hom b c -> [Hom a b] -> [Hom a c]
mapHom f functions = map (f .) functions  -- Function composition

-- Natural transformations between Hom-sets
-- Example: pre-composition with a function g :: A -> B
-- gives a natural transformation: Hom(B, C) -> Hom(A, C)
preCompose :: (a -> b) -> (b -> c) -> (a -> c)
preCompose g f = f . g

-- Example: post-composition with a function f :: B -> C
-- gives a natural transformation: Hom(A, B) -> Hom(A, C)
postCompose :: (b -> c) -> (a -> b) -> (a -> c)
postCompose f g = f . g

-- Comprehensive demonstration of Hom-sets in Cartesian Closed Categories
main :: IO ()
main = do
  putStrLn "=== Hom-sets in Cartesian Closed Categories ==="
  putStrLn ""

  -- 1. Currying isomorphism demonstration
  putStrLn "1. Currying Isomorphism: Hom(A × B, C) ≅ Hom(A, C^B)"
  -- Left side: Hom(A × B, C) - functions from pairs to C
  let addPair :: (Int, Int) -> Int
      addPair (x, y) = x + y

  -- Right side: Hom(A, C^B) - functions from A to functions (B -> C)
  let addCurried :: Int -> (Int -> Int)
      addCurried = curry addPair

  -- They represent the same mathematical relationship
  print $ addPair (5, 3)        -- 8
  print $ addCurried 5 3        -- 8
  -- The isomorphism is witnessed by curry/uncurry
  print $ uncurry addCurried (5, 3)  -- 8
  putStrLn ""

  -- 2. Higher-order function manipulation
  putStrLn "2. Higher-order Function Manipulation"
  let functions :: [Int -> Int]
      functions = [(+1), (*2), (\x -> x*x)]  -- List of Hom(Int, Int)

  let showFunction :: Int -> String
      showFunction = show  -- Hom(Int, String)

  -- Apply showFunction after each function: [Hom(Int, String)]
  let stringFunctions = mapHom showFunction functions

  -- Test the transformed functions
  putStrLn "Transformed functions applied to 5:"
  mapM_ (\f -> print (f 5)) stringFunctions  -- "6", "10", "25"
  putStrLn ""

  -- 3. Exponential objects as function spaces
  putStrLn "3. Exponential Objects as Function Spaces"
  -- B^A represents the space of all functions A -> B
  let intToString :: [Int -> String]  -- Some functions in String^Int
      intToString = [show, \x -> "num: " ++ show x, \x -> replicate x 'x']

  -- Functions can be manipulated like any other data
  let selectedFunction = head intToString  -- Pick a function
  putStrLn $ "Selected function(42) = " ++ selectedFunction 42  -- "42"

  -- Composition creates new functions in the space
  let composed = map (("Result: " ++) .) intToString  -- [String^Int]
  putStrLn $ "Composed function(7) = " ++ head composed 7  -- "Result: 7"
  putStrLn ""

  -- 4. Natural transformations demonstration
  putStrLn "4. Natural Transformations Between Hom-sets"
  let doubleIt :: Int -> Int
      doubleIt = (*2)

  let originalFunc :: Int -> String
      originalFunc = \x -> "Value: " ++ show x

  -- Pre-composition: apply doubleIt before originalFunc
  let preComposed = preCompose doubleIt originalFunc
  putStrLn $ "Original function(5) = " ++ originalFunc 5      -- "Value: 5"
  putStrLn $ "Pre-composed function(5) = " ++ preComposed 5   -- "Value: 10"

  -- Post-composition: apply show after (+1)
  let addOne :: Int -> Int
      addOne = (+1)
  let postComposed = postCompose show addOne
  putStrLn $ "Post-composed function(5) = " ++ postComposed 5  -- "6"
  putStrLn ""

  -- 5. The fundamental theorem
  putStrLn "5. Fundamental Theorem: Hom(A,B) ≅ B^A"
  putStrLn "In Haskell (a Cartesian Closed Category):"
  putStrLn "- Types are objects"
  putStrLn "- Functions (A -> B) are morphisms in Hom(A,B)"
  putStrLn "- Function types (A -> B) are also objects (exponentials B^A)"
  putStrLn "- This makes functions first-class: they can be arguments and results"
  putStrLn ""

  -- 6. Demonstrating functions as both morphisms and objects
  putStrLn "6. Functions as Both Morphisms and Objects"
  let functionAsMorphism :: Int -> String
      functionAsMorphism = show  -- This is a morphism in Hom(Int, String)

  -- The same function type as objects that can be manipulated
  let functionsAsObjects :: [Int -> String]
      functionsAsObjects = [functionAsMorphism, \x -> "Num: " ++ show x]

  -- Higher-order function that processes functions (treating them as objects)
  let testAllFunctions :: [Int -> String] -> Int -> [String]
      testAllFunctions funcs x = map ($ x) funcs

  let results = testAllFunctions functionsAsObjects 42
  putStrLn $ "Testing functions as objects with 42: " ++ show results
  -- ["42", "Num: 42"]
