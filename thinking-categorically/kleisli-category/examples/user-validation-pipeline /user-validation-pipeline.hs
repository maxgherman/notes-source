data User = User String Int String deriving Show
data ValidationError = EmptyName | InvalidAge | InvalidEmail deriving Show

-- Kleisli arrows for validation (using Either for error handling)
validateName :: String -> Either ValidationError String
validateName "" = Left EmptyName
validateName name = Right name

validateAge :: String -> Either ValidationError Int
validateAge ageStr =
    case reads ageStr of
        [(age, "")] | age >= 0 && age <= 150 -> Right age
        _ -> Left InvalidAge

validateEmail :: String -> Either ValidationError String
validateEmail email
    | '@' `elem` email = Right email
    | otherwise = Left InvalidEmail

-- Kleisli composition for user creation
createUser :: (String, String, String) -> Either ValidationError User
createUser (name, ageStr, email) = do
    validName <- validateName name
    validAge <- validateAge ageStr
    validEmail <- validateEmail email
    return $ User validName validAge validEmail

-- Usage
main :: IO ()
main = do
    print $ createUser ("Alice", "25", "alice@example.com")  -- Right (User "Alice" 25 "alice@example.com")
    print $ createUser ("", "25", "alice@example.com")      -- Left EmptyName
