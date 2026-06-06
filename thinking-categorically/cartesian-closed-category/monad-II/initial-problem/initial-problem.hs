import Data.Char (toUpper, toLower)
import Text.Read (readMaybe)

-- Configuration for transformations
data Config = Config
  { ageThreshold :: Int
  , locale :: String
  , requireValidAge :: Bool
  } deriving (Show)

-- Transformation errors
data TransformationError
  = InvalidAge String
  | MissingField String
  | AgeOutOfRange Int
  deriving (Show, Eq)

-- Result type after transformation
data Person = Person
  { personName :: String
  , personAge :: Int
  , personAgeGroup :: String
  , transformationHistory :: [String]
  } deriving (Show)

-- Simple Result type (similar to Either but more explicit)
data Result e a = Error e | Ok a deriving (Show, Eq)

instance Functor (Result e) where
  fmap _ (Error e) = Error e
  fmap f (Ok a) = Ok (f a)

instance Applicative (Result e) where
  pure = Ok
  Error e <*> _ = Error e
  Ok f <*> something = fmap f something

instance Monad (Result e) where
  return = pure
  Error e >>= _ = Error e
  Ok a >>= f = f a

-- Parse age with validation
parseAge :: Config -> String -> Result TransformationError Int
parseAge config ageStr =
  case readMaybe ageStr of
    Nothing -> Error (InvalidAge ageStr)
    Just age ->
      if requireValidAge config && (age < 0 || age > 150)
      then Error (AgeOutOfRange age)
      else Ok age

-- Categorize age based on config threshold
categorizeAge :: Config -> Int -> Result TransformationError String
categorizeAge config age =
  Ok $ if age > ageThreshold config then "old" else "young"

-- Localize name based on config locale
localizeName :: Config -> String -> Result TransformationError String
localizeName config name =
  Ok $ case locale config of
    "de" -> map toUpper name
    "en" -> map toLower name
    _    -> name

-- Monadic transformation pipeline (simplified without ReaderT)
transformPerson :: Config -> [(String, String)] -> Result TransformationError Person
transformPerson config row = do
  -- Extract required fields
  name <- case lookup "name" row of
    Nothing -> Error (MissingField "name")
    Just n  -> Ok n

  ageStr <- case lookup "age" row of
    Nothing -> Error (MissingField "age")
    Just a  -> Ok a

  -- Sequential monadic composition
  age <- parseAge config ageStr
  ageGroup <- categorizeAge config age
  localizedName <- localizeName config name

  -- Build result with transformation history
  return Person
    { personName = localizedName
    , personAge = age
    , personAgeGroup = ageGroup
    , transformationHistory =
        [ "Parsed age: " ++ show age
        , "Categorized as: " ++ ageGroup
        , "Localized name: " ++ localizedName
        ]
    }

-- Process CSV data with monadic error handling
transformCSVData :: [[(String, String)]] -> Config -> Result [TransformationError] [Person]
transformCSVData rows config =
  let results = map (transformPerson config) rows
      (errors, successes) = partitionResults results
  in if null errors
     then Ok successes
     else Error errors
  where
    partitionResults :: [Result e a] -> ([e], [a])
    partitionResults = foldr partitionResult ([], [])
      where
        partitionResult (Error e) (errs, succs) = (e:errs, succs)
        partitionResult (Ok a) (errs, succs) = (errs, a:succs)

-- Simple CSV parser
parseCSV :: String -> [[(String, String)]]
parseCSV csv =
  case lines csv of
    [] -> []
    (header:rows) ->
      let keys = splitOn ',' header
          parseRow row = zip keys (splitOn ',' row)
      in map parseRow rows
  where
    splitOn :: Char -> String -> [String]
    splitOn delim = words . map (\c -> if c == delim then ' ' else c)

-- Example usage
main :: IO ()
main = do
  let csvData = "name,age\nAlice,25\nBob,invalid\nCharlie,35"
      rows = parseCSV csvData
      config = Config { ageThreshold = 30, locale = "de", requireValidAge = True }
      result = transformCSVData rows config

  case result of
    Error errors -> do
      putStrLn "Transformation errors:"
      mapM_ print errors
    Ok people -> do
      putStrLn "Transformed data:"
      mapM_ print people
