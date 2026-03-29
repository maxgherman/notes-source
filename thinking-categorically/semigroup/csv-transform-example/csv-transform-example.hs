import Data.List.Split (splitOn)
import Data.Semigroup (Sum(..), getSum)

-- Person data type
data Person = Person
  { name :: String
  , age :: Int
  , totalAge :: Maybe Int
  , count :: Maybe Int
  } deriving (Show, Eq)

-- Semigroup instance for Person
instance Semigroup Person where
  p1 <> p2 = Person
    { name = name p1 ++ " & " ++ name p2
    , age = max (age p1) (age p2)
    , totalAge = Just $ (maybe (age p1) id (totalAge p1)) + (maybe (age p2) id (totalAge p2))
    , count = Just $ (maybe 1 id (count p1)) + (maybe 1 id (count p2))
    }

-- Stats data type for summary statistics
data Stats = Stats
  { statsTotalAge :: Int
  , statsCount :: Int
  , statsNames :: [String]
  } deriving (Show, Eq)

-- Semigroup instance for Stats
instance Semigroup Stats where
  s1 <> s2 = Stats
    { statsTotalAge = statsTotalAge s1 + statsTotalAge s2
    , statsCount = statsCount s1 + statsCount s2
    , statsNames = statsNames s1 ++ statsNames s2
    }

-- CSV parsing function
readCSV :: String -> [Person]
readCSV csvData =
  let allLines = lines csvData
  in case allLines of
       [] -> []
       (header:rows) ->
         let keys = splitOn "," header
             parseRow row =
               let values = splitOn "," row
                   rowMap = zip keys values
                   n = lookup "name" rowMap
                   a = lookup "age" rowMap >>= readMaybe
               in case (n, a) of
                    (Just nameVal, Just ageVal) ->
                      Just $ Person nameVal ageVal Nothing Nothing
                    _ -> Nothing
         in map (\r -> case parseRow r of
                         Just p -> p
                         Nothing -> error "Malformed row") rows
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
                    [(x, "")] -> Just x
                    _ -> Nothing

-- Combine all elements using semigroup
combineAll :: Semigroup a => [a] -> Maybe a
combineAll [] = Nothing
combineAll (x:xs) = Just $ foldl (<>) x xs

-- Convert Person to Stats
personToStats :: Person -> Stats
personToStats p = Stats (age p) 1 [name p]

-- Main function
main :: IO ()
main = do
  let csvData = "name,age\nAlice,25\nBob,40\nCharlie,35"
  let dataList = readCSV csvData

  putStrLn "Original data:"
  mapM_ print dataList

  -- 1. Combine all persons into one record
  case combineAll dataList of
    Just combinedPerson -> do
      putStrLn "\nCombined person:"
      print combinedPerson
    Nothing -> putStrLn "No data to combine"

  -- 2. Sum all ages using built-in Sum semigroup
  let ages = map age dataList
  let ageSums = map (Sum . fromIntegral :: Int -> Sum Integer) ages  -- Convert to Sum semigroup
  case combineAll ageSums of
    Just totalSum -> putStrLn $ "\nTotal age (using Sum semigroup): " ++ show (getSum totalSum)
    Nothing -> putStrLn "No ages to sum"

  -- 3. Collect all names using semigroup
  let names = map ((: []) . name) dataList  -- Convert to list of lists
  case combineAll names of
    Just allNames -> putStrLn $ "All names: " ++ show allNames
    Nothing -> putStrLn "No names to collect"

  -- 4. Process multiple CSV datasets and combine them
  let csvData2 = "name,age\nDave,28\nEve,32"
  let csvData3 = "name,age\nFrank,45\nGrace,29"
  let data2 = readCSV csvData2
  let data3 = readCSV csvData3

  let allDatasets = dataList ++ data2 ++ data3
  putStrLn "\nCombined datasets:"
  mapM_ print allDatasets

  -- 5. Create summary statistics using semigroups
  let statsFromData = map personToStats allDatasets
  case combineAll statsFromData of
    Just overallStats -> do
      putStrLn "\nOverall statistics:"
      print overallStats
      let avgAge = fromIntegral (statsTotalAge overallStats) / fromIntegral (statsCount overallStats) :: Double
      putStrLn $ "Average age: " ++ show avgAge

      -- Also demonstrate Sum semigroup for total age calculation
      let allAges = map (Sum . fromIntegral . age :: Person -> Sum Integer) allDatasets
      case combineAll allAges of
        Just totalAgeSum -> putStrLn $ "Total age (Sum semigroup): " ++ show (getSum totalAgeSum)
        Nothing -> putStrLn "No ages to sum with Sum semigroup"
    Nothing -> putStrLn "No stats to combine"
