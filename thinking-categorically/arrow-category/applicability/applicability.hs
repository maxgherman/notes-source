{-# LANGUAGE Arrows #-}
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- Basic Arrow example: Function arrows
-- Functions themselves form an Arrow
basicArrowExample :: IO ()
basicArrowExample = do
  putStrLn "=== Basic Function Arrows ==="

  -- arr lifts a pure function to an arrow
  let addOne = arr (+1) :: (->) Integer Integer
  let double = arr (*2) :: (->) Integer Integer
  let toString = arr show :: (->) Integer String

  -- Arrow composition using >>>
  let pipeline = addOne >>> double >>> toString

  print $ pipeline (5 :: Integer)  -- "12"

  -- First and second operate on pairs (product types)
  let pairTransform = first addOne >>> second double
  print $ pairTransform (10 :: Integer, 20 :: Integer)  -- (11, 40)

-- Arrow laws demonstration
arrowLawsExample :: IO ()
arrowLawsExample = do
  putStrLn "\n=== Arrow Laws ==="

  -- Law 1: arr id = id
  print $ (arr id :: (->) Integer Integer) (42 :: Integer)  -- 42
  print $ id (42 :: Integer)        -- 42 (same result)

  -- Law 2: arr (g . f) = arr f >>> arr g
  print $ (arr ((+3) . (*2)) :: (->) Integer Integer) (5 :: Integer)      -- 13
  print $ (arr (*2) >>> arr (+3) :: (->) Integer Integer) (5 :: Integer)  -- 13 (same result)

  -- Law 3: first (arr f) = arr (first f)
  print $ (first (arr (+1) :: (->) Integer Integer)) (10 :: Integer, 20 :: Integer)       -- (11, 20)
  print $ (arr (first (+1)) :: (->) (Integer, Integer) (Integer, Integer)) (10 :: Integer, 20 :: Integer)       -- (11, 20) (same result)

-- Arrow notation example (syntactic sugar)
arrowNotationExample :: IO ()
arrowNotationExample = do
  putStrLn "\n=== Arrow Notation ==="

  -- Using arrow notation for cleaner syntax
  let computation :: (->) Integer String
      computation = proc x -> do
        doubled <- arr (*2) -< x
        result <- arr show -< doubled
        returnA -< "Result: " ++ result

  print $ computation (21 :: Integer)  -- "Result: 42"

  -- More complex arrow computation with conditionals
  let conditionalCompute :: (->) Integer String
      conditionalCompute = proc x -> do
        doubled <- arr (*2) -< x
        if doubled > 10
          then arr ("Big: " ++) -< show doubled
          else arr ("Small: " ++) -< show doubled

  print $ conditionalCompute (3 :: Integer)   -- "Small: 6"
  print $ conditionalCompute (8 :: Integer)   -- "Big: 16"

-- Custom arrow instance for logging computations
newtype LogArrow a b = LogArrow (a -> (b, [String]))

runLogArrow :: LogArrow a b -> a -> (b, [String])
runLogArrow (LogArrow f) = f

instance Category LogArrow where
  id = LogArrow $ \x -> (x, [])
  (LogArrow g) . (LogArrow f) = LogArrow $ \x ->
    let (y, logs1) = f x
        (z, logs2) = g y
    in (z, logs1 ++ logs2)

instance Arrow LogArrow where
  arr f = LogArrow $ \x -> (f x, ["Applied function"])
  first (LogArrow f) = LogArrow $ \(x, z) ->
    let (y, logs) = f x
    in ((y, z), logs)
  second (LogArrow f) = LogArrow $ \(z, x) ->
    let (y, logs) = f x
    in ((z, y), logs)

-- Using custom LogArrow
logArrowExample :: IO ()
logArrowExample = do
  putStrLn "\n=== Custom LogArrow ==="

  let addOneLog :: LogArrow Integer Integer
      addOneLog = LogArrow $ \x -> (x + 1, ["Added 1"])
  let doubleLog :: LogArrow Integer Integer
      doubleLog = LogArrow $ \x -> (x * 2, ["Doubled"])
  let computation = addOneLog >>> doubleLog

  let (result, logs) = runLogArrow computation (5 :: Integer)
  print result  -- 12
  print logs    -- ["Added 1", "Doubled"]

main :: IO ()
main = do
  basicArrowExample
  arrowLawsExample
  arrowNotationExample
  logArrowExample