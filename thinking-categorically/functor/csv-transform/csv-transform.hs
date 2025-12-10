module Main where

-- CSV Transform example in Haskell using Functor (List functor)
import Data.Char (toUpper)
import Data.List.Split (splitOn)

data Person = Person
  { name     :: String
  , age      :: Int
  , ageGroup :: String
  } deriving (Show)

-- Parse CSV string into list of Person objects
readCSV :: String -> [Person]
readCSV csv =
  case lines csv of
    [] -> []
    header:rows ->
      let keys = splitOn "," header
          parseRow row =
            let values = splitOn "," row
                rowMap = zip keys values
                n = lookup "name" rowMap
                a = lookup "age" rowMap >>= Just . read
            in case (n, a) of
                 (Just nameVal, Just ageVal) ->
                   let group = if ageVal > 30 then "old" else "young"
                   in Just $ Person (map toUpper nameVal) ageVal group
                 _ -> Nothing
      in map (\r -> case parseRow r of
                      Just p -> p
                      Nothing -> error "Malformed row") rows

main :: IO ()
main = do
  let csv = "name,age\nAlice,25\nBob,40"
  let people = readCSV csv
  mapM_ print people
