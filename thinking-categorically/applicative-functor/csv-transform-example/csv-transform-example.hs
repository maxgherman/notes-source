import Data.Char (toUpper, toLower)
import Control.Applicative (liftA3)

-- Custom splitOn (safe and total)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim xs = go xs [[]]
  where
    go [] acc = reverse (map reverse acc)
    go (c:cs) (a:as)
      | c == delim = go cs ([] : a : as)
      | otherwise  = go cs ((c : a) : as)
    go _ [] = error "Unexpected empty accumulator in splitOn"

-- Configuration passed via Reader
data Config = Config
  { ageThreshold :: Int
  , locale       :: String
  }

-- Output type
data Person = Person
  { name     :: String
  , age      :: Int
  , ageGroup :: String
  } deriving (Show)

-- Per-row transformation using Reader
type Reader r a = r -> a

transformPerson :: String -> String -> Reader Config Person
transformPerson rawName rawAge = liftA3 Person
  localizedName
  parsedAge
  grouped
  where
    parsedAge :: Reader Config Int
    parsedAge = pure (read rawAge)

    localizedName :: Reader Config String
    localizedName cfg =
      case locale cfg of
        "de" -> map toUpper rawName
        "en" -> map toLower rawName
        _    -> rawName

    grouped :: Reader Config String
    grouped cfg =
      let ageVal = read rawAge
       in if ageVal > ageThreshold cfg then "old" else "young"

readCSV :: String -> Reader Config [Person]
readCSV csv =
  case lines csv of
    [] -> const []  -- return an empty Reader
    (header : rows) ->
      let keys = splitOn ',' header
          parseRow row =
            let values = splitOn ',' row
                rowMap = zip keys values
                mName = lookup "name" rowMap
                mAge  = lookup "age" rowMap
             in case (mName, mAge) of
                  (Just n, Just a) -> transformPerson n a
                  _ -> error $ "Malformed row: " ++ row
      in mapM parseRow rows

main :: IO ()
main = do
  let csv = "name,age\nAlice,25\nBob,40"
  let config = Config { ageThreshold = 30, locale = "de" }
  let people = readCSV csv config
  mapM_ print people
