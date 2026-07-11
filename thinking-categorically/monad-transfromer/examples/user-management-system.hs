{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T

-- Domain types
data User = User
  { userId :: Int,
    userName :: Text,
    userEmail :: Text
  }
  deriving (Show, Eq)

data Config = Config
  { dbConnection :: Text,
    maxRetries :: Int,
    logLevel :: Text
  }
  deriving (Show)

data AppError
  = UserNotFound Int
  | DatabaseError Text
  | ValidationError Text
  deriving (Show, Eq)

type LogEntry = Text

-- Our transformer stack: ReaderT + ExceptT + WriterT + IO
type AppM = ReaderT Config (ExceptT AppError (WriterT [LogEntry] IO))

-- Helper function to run our app
runApp :: Config -> AppM a -> IO (Either AppError a, [LogEntry])
runApp config action =
  runWriterT $ runExceptT $ runReaderT action config

-- Core business operations
validateUser :: User -> AppM User
validateUser user = do
  tell ["Validating user: " <> userName user]
  _ <- ask

  if T.length (userName user) < 3
    then throwError (ValidationError "Username too short")
    else do
      tell ["User validation passed"]
      return user

saveUser :: User -> AppM User
saveUser user = do
  config <- ask
  tell ["Saving user to: " <> dbConnection config]

  -- Simulate potential database failure
  liftIO $ putStrLn $ "Connecting to: " ++ T.unpack (dbConnection config)

  if userId user == 999
    then throwError (DatabaseError "Database connection failed")
    else do
      tell ["User saved successfully"]
      return user

findUser :: Int -> AppM User
findUser uid = do
  tell ["Looking up user with ID: " <> T.pack (show uid)]

  -- Simulate database lookup
  if uid == 1
    then return $ User 1 "alice" "alice@example.com"
    else throwError (UserNotFound uid)

-- Complex operation combining multiple effects
processUser :: Int -> Text -> Text -> AppM User
processUser uid name email = do
  tell ["Starting user processing"]
  _ <- ask

  let newUser = User uid name email

  -- Chain operations that might fail
  validatedUser <- validateUser newUser
  savedUser <- saveUser validatedUser

  tell ["User processing completed successfully"]
  return savedUser

updateExistingUser :: Int -> Text -> AppM User
updateExistingUser uid newName = do
  tell ["Updating user: " <> T.pack (show uid)]

  -- Find existing user (might fail)
  existingUser <- findUser uid

  -- Update and save (might fail)
  let updatedUser = existingUser {userName = newName}
  validatedUser <- validateUser updatedUser
  saveUser validatedUser

-- Example usage
main :: IO ()
main = do
  let config = Config "postgresql://localhost/mydb" 3 "INFO"

  putStrLn "=== Creating new user ==="
  (result1, logs1) <- runApp config $ processUser 42 "bob" "bob@example.com"
  putStrLn "Logs:"
  mapM_ (putStrLn . ("  " ++) . T.unpack) logs1
  putStrLn $ "Result: " ++ show result1

  putStrLn "\n=== Updating existing user ==="
  (result2, logs2) <- runApp config $ updateExistingUser 1 "alice_updated"
  putStrLn "Logs:"
  mapM_ (putStrLn . ("  " ++) . T.unpack) logs2
  putStrLn $ "Result: " ++ show result2

  putStrLn "\n=== Error case ==="
  (result3, logs3) <- runApp config $ processUser 999 "baduser" "bad@example.com"
  putStrLn "Logs:"
  mapM_ (putStrLn . ("  " ++) . T.unpack) logs3
  putStrLn $ "Result: " ++ show result3
