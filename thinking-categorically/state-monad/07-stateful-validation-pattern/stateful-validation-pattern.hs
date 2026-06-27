import Control.Monad.State
import qualified Data.Map as Map

-- Validation state containing errors and validated context
data ValidationState = ValidationState
  { errors :: [String]
  , context :: Map.Map String String
  } deriving (Show)

-- Validate that a field is required (not empty)
validateRequired :: String -> String -> State ValidationState Bool
validateRequired field value = do
    state' <- get
    if null value
        then do
            put $ state' { errors = errors state' ++ [field ++ " is required"] }
            return False
        else do
            put $ state' { context = Map.insert field value (context state') }
            return True

-- Validate email format
validateEmail :: String -> State ValidationState Bool
validateEmail email = do
    state' <- get
    let isValid = '@' `elem` email && '.' `elem` email
    if not isValid
        then do
            put $ state' { errors = errors state' ++ ["Invalid email format"] }
            return False
        else return True

-- Validate a complete user
validateUser :: String -> String -> State ValidationState Bool
validateUser name email = do
    nameValid <- validateRequired "name" name
    emailValid <- validateRequired "email" email
    emailFormatValid <- if emailValid then validateEmail email else return False
    return $ nameValid && emailValid && emailFormatValid

-- Usage example
main :: IO ()
main = do
    let initialState = ValidationState [] Map.empty
        (isValid, finalState) = runState (validateUser "John" "john@example.com") initialState
    putStrLn $ "Is valid: " ++ show isValid
    putStrLn $ "Errors: " ++ show (errors finalState)
    putStrLn $ "Context: " ++ show (context finalState)
