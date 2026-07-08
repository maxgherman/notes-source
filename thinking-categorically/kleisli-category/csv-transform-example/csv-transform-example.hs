import Control.Monad

-- Domain types
data Person = Person
  { name :: String
  , age :: Int
  , email :: String
  } deriving Show

-- Kleisli arrows for CSV field transformations
parseAge :: String -> Maybe Int
parseAge str = case reads str of
  [(age', "")] -> Just age'
  _ -> Nothing

validateAge :: Int -> Maybe Int
validateAge age'
  | age' >= 0 && age' <= 150 = Just age'
  | otherwise = Nothing

parseEmail :: String -> Maybe String
parseEmail email'
  | '@' `elem` email' = Just email'
  | otherwise = Nothing

-- Creating Person from validated components - also a Kleisli arrow
createPerson :: String -> String -> String -> Maybe Person
createPerson nameStr ageStr emailStr = do
  validAge <- parseAge ageStr >>= validateAge
  validEmail <- parseEmail emailStr
  return $ Person nameStr validAge validEmail

-- Kleisli composition for complete CSV row transformation
transformCSVRow :: [String] -> Maybe Person
transformCSVRow [nameStr, ageStr, emailStr] =
  createPerson nameStr ageStr emailStr
transformCSVRow _ = Nothing

-- Alternative using Kleisli composition operators
-- Individual field processors as Kleisli arrows
processAge :: String -> Maybe Int
processAge = parseAge >=> validateAge

processEmail :: String -> Maybe String
processEmail = parseEmail

-- Complete pipeline
processCSVRow :: [String] -> Maybe Person
processCSVRow [name', age', email'] = do
  validAge <- processAge age'
  validEmail <- processEmail email'
  return $ Person name' validAge validEmail
processCSVRow _ = Nothing

-- Usage example
main :: IO ()
main = do
  let csvRows =  [
        ["Alice", "25", "alice@example.com"],
        ["Bob", "-5", "bob@example.com"],    -- Invalid age
        ["Charlie", "30", "invalid-email"],  -- Invalid email
        ["Diana", "28", "diana@example.com"]]

  let results = map transformCSVRow csvRows
  let results' = map processCSVRow csvRows
  mapM_ print results
  mapM_ print results'

  -- Output:
  -- Just (Person {name = "Alice", age = 25, email = "alice@example.com"})
  -- Nothing
  -- Nothing
  -- Just (Person {name = "Diana", age = 28, email = "diana@example.com"})

