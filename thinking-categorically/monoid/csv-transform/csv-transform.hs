{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split (splitOn)
import Data.Monoid (Sum(..), getSum)
import Data.Maybe (fromMaybe, mapMaybe)

-- Person data type with safe defaults
data Person = Person
  { personName :: String
  , personAge :: Int
  , personTotalAge :: Maybe Int
  , personCount :: Maybe Int
  } deriving (Show, Eq)

-- Monoid instance for Person (includes identity element)
instance Semigroup Person where
  p1 <> p2 = Person
    { personName = if personName p1 == "" then personName p2
                   else if personName p2 == "" then personName p1
                   else personName p1 ++ " & " ++ personName p2
    , personAge = max (personAge p1) (personAge p2)
    , personTotalAge = Just $ (fromMaybe (personAge p1) (personTotalAge p1)) +
                              (fromMaybe (personAge p2) (personTotalAge p2))
    , personCount = Just $ (fromMaybe 1 (personCount p1)) +
                           (fromMaybe 1 (personCount p2))
    }

instance Monoid Person where
  mempty = Person "" 0 (Just 0) (Just 0)  -- Identity element

-- Statistics data type for comprehensive analysis
data Stats = Stats
  { statsTotalAge :: Int
  , statsCount :: Int
  , statsNames :: [String]
  , statsAverageAge :: Double
  } deriving (Show, Eq)

-- Monoid instance for Stats
instance Semigroup Stats where
  s1 <> s2 =
    let totalAge = statsTotalAge s1 + statsTotalAge s2
        count = statsCount s1 + statsCount s2
        avgAge = if count > 0 then fromIntegral totalAge / fromIntegral count else 0
    in Stats totalAge count (statsNames s1 ++ statsNames s2) avgAge

instance Monoid Stats where
  mempty = Stats 0 0 [] 0  -- Identity element for safe empty handling

-- Safe CSV parsing function
readCSV :: String -> [Person]
readCSV csvData
  | null (filter (not . null) (lines csvData)) = []  -- Safe handling of empty input
  | otherwise =
      let allLines = lines csvData
      in case allLines of
           [] -> []
           [_] -> []  -- Header only
           (header:rows) ->
             let keys = splitOn "," header
                 parseRow row = do
                   let values = splitOn "," row
                       rowMap = zip keys values
                   nameVal <- lookup "name" rowMap
                   ageStr <- lookup "age" rowMap
                   ageVal <- readMaybe ageStr
                   return $ Person nameVal ageVal Nothing Nothing
             in mapMaybe parseRow rows
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
                    [(x, "")] -> Just x
                    _ -> Nothing

-- Safe fold function using monoids (no Maybe needed!)
foldMonoid :: Monoid a => [a] -> a
foldMonoid = mconcat  -- Always safe, empty list becomes mempty

-- Safe map-fold operation
foldMapSafe :: Monoid b => (a -> b) -> [a] -> b
foldMapSafe f = foldMap f  -- Always safe with monoids

-- Convert Person to Stats
personToStats :: Person -> Stats
personToStats p =
  let age = personAge p
  in Stats age 1 [personName p] (fromIntegral age)

-- Safe dataset processing
processDatasetSafely :: String -> Stats
processDatasetSafely csvData =
  let persons = readCSV csvData
  in foldMapSafe personToStats persons

-- Error recovery with identity elements
processWithRetry :: [String] -> Stats
processWithRetry csvInputs =
  let tryProcess csv = case readCSV csv of
        [] -> mempty  -- Empty contributes identity element
        persons -> foldMapSafe personToStats persons
  in foldMonoid (map tryProcess csvInputs)

-- Main demonstration
main :: IO ()
main = do
  let csvData1 = "name,age\nAlice,25\nBob,40\nCharlie,35"
  let csvData2 = "name,age\nDave,28\nEve,32"
  let csvData3 = ""  -- Empty CSV - safe with monoids!
  let csvData4 = "name,age\nFrank,45\nGrace,29"

  let data1 = readCSV csvData1
  let data2 = readCSV csvData2
  let data3 = readCSV csvData3  -- Empty list
  let data4 = readCSV csvData4

  putStrLn "Dataset 1:"
  mapM_ print data1
  putStrLn $ "Dataset 2: " ++ show (length data2) ++ " records"
  putStrLn $ "Dataset 3 (empty): " ++ show (length data3) ++ " records"  -- Safe empty handling
  putStrLn $ "Dataset 4: " ++ show (length data4) ++ " records"

  -- 1. Safe combination of all persons (empty datasets don't break anything)
  let combinedPersons = map foldMonoid [data1, data2, data3, data4]
  let finalCombinedPerson = foldMonoid combinedPersons

  putStrLn "\nCombined person:"
  print finalCombinedPerson

  -- 2. Safe age summation using Sum monoid (works even with empty datasets)
  let allPersons = data1 ++ data2 ++ data3 ++ data4  -- Includes empty list safely
  let allAges = map (Sum . fromIntegral . personAge :: Person -> Sum Int) allPersons
  let totalAge = getSum $ foldMonoid allAges

  putStrLn $ "Total age: " ++ show totalAge

  -- 3. Safe name collection using list concatenation monoid
  let allNames = foldMapSafe (\p -> [personName p]) allPersons
  putStrLn $ "All names: " ++ show allNames

  -- 4. Safe dataset merging with automatic empty handling
  let allDatasets = foldMonoid [data1, data2, data3, data4]  -- Empty lists handled automatically
  putStrLn $ "Combined datasets: " ++ show (length allDatasets) ++ " total records"

  -- 5. Comprehensive statistics with safe empty handling
  let overallStats = foldMapSafe personToStats allDatasets
  putStrLn "\nOverall statistics:"
  print overallStats
  putStrLn $ "Processed " ++ show (statsCount overallStats) ++
             " records with average age " ++ show (statsAverageAge overallStats)

  -- 6. Safe pipeline operations
  let pipelineResults = map processDatasetSafely
        [ "name,age\nAlice,25"
        , ""  -- Empty CSV
        , "name,age"  -- Header only
        , "name,age\nBob,30\nCharlie,35"
        ]

  let finalPipelineResult = foldMonoid pipelineResults
  putStrLn $ "Pipeline result: " ++ show (statsCount finalPipelineResult) ++ " records processed"

  -- 7. Error recovery with identity elements
  let robustResult = processWithRetry
        [ "name,age\nAlice,25"
        , ""  -- Empty - safe
        , "invalid,csv,format"  -- Might fail - safe
        , "name,age\nBob,30"
        ]

  putStrLn $ "Robust processing result: " ++ show (statsCount robustResult) ++ " records"
