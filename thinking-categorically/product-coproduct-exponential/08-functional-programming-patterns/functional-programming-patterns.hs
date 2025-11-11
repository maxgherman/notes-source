import Control.Monad ((>=>))

type UserProfile = (String, Int, String)  -- (name, age, email)

data DatabaseConfig = DatabaseConfig
  { host :: String
  , port :: Int
  , database :: String
  }

data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show)

data DatabaseError where
  DatabaseError :: String -> DatabaseError
  deriving (Eq, Show)

-- Coproducts as sum types
data Result a b = Success a | Error b
data ParseResult = ParsedNumber Int | ParsedString String | ParseError String

-- Exponentials as function types
type Validator a = a -> Either String a
type Transform a b = a -> b
type Predicate a = a -> Bool

-- Higher-order functions work naturally with products/coproducts/exponentials
processResults :: [Result User DatabaseError] -> IO ()
processResults results = do
  let (users, errors) = partitionResults results
  mapM_ saveUser users
  mapM_ logError errors

partitionResults :: [Result a b] -> ([a], [b])
partitionResults = foldr separate ([], [])
  where
    separate (Success a) (as, bs) = (a:as, bs)
    separate (Error b) (as, bs) = (as, b:bs)

-- Currying and partial application with exponentials
validateAndSave :: Validator User -> [User] -> IO [Result User String]
validateAndSave validator = mapM (validate >=> save)
  where
    validate user = case validator user of
      Left err -> return (Error err)
      Right validUser -> return (Success validUser)

    save (Error e) = return (Error e)
    save (Success u) = do
      saveUser u
      return (Success u)

saveUser :: User -> IO ()
saveUser u = putStrLn ("Saved user: " ++ show u)

logError :: DatabaseError -> IO ()
logError (DatabaseError msg) = putStrLn ("DB error: " ++ msg)