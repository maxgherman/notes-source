{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import Data.Void
import Data.Functor.Contravariant hiding (Predicate)
import Data.Time

-- Decidable typeclass definition (if not already defined)
class Contravariant f => Decidable f where
  lose :: (a -> Void) -> f a
  choose :: (a -> Either b c) -> f b -> f c -> f a

-- Validation that can discriminate between different error types
data ValidationError
  = NameError String
  | EmailError String
  | AgeError String
  deriving (Show, Eq)

newtype Validator a = Validator (a -> [ValidationError])

instance Contravariant Validator where
  contramap f (Validator validate) = Validator (validate . f)

instance Decidable Validator where
  lose f = Validator (absurd . f)

  choose split (Validator vb) (Validator vc) =
    Validator $ \a -> case split a of
      Left b -> vb b
      Right c -> vc c

-- Helper function to run validator
runValidator :: Validator a -> a -> [ValidationError]
runValidator (Validator validate) = validate

-- Specific validators
emailValidator :: Validator String
emailValidator = Validator $ \email ->
  ([EmailError "Invalid email format - must contain @ and ." | not ('@' `elem` email && '.' `elem` email)])

ageValidator :: Validator Int
ageValidator = Validator $ \age ->
  ([AgeError $ "Invalid age: " ++ show age ++ " (must be 18-120)" | not (age >= 18 && age <= 120)])

-- User data that needs different validation strategies
data UserField = Email String | Age Int
  deriving (Show)

-- Route validation based on field type
fieldValidator :: Validator UserField
fieldValidator = choose fieldSplit emailValidator ageValidator
  where
    fieldSplit = \case
      Email s -> Left s
      Age n -> Right n

-- Complex form with multiple field types
data FormData = FormData
  { fields :: [UserField]
  , submitTime :: UTCTime
  } deriving (Show)

-- Validate entire form by routing each field appropriately
formValidator :: Validator FormData
formValidator = contramap fields (listValidator fieldValidator)
  where
    listValidator :: Validator a -> Validator [a]
    listValidator (Validator validate) = Validator (concatMap validate)

-- Additional validators for demonstration
nameValidator :: Validator String
nameValidator = Validator $ \name ->
  [ NameError "Name must be at least 2 characters and contain only letters and spaces"
  | not (length name >= 2 && all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ") name)
  ]

-- Example using 'lose' for impossible/uninhabited types
data Impossible  -- uninhabited type (no constructors)

-- This validator handles the impossible case using 'lose'
impossibleValidator :: Validator Impossible
impossibleValidator = lose (\case {})  -- empty case analysis proves impossibility

-- Optional values have two inhabited branches, so the absent branch gets a
-- real validator rather than being incorrectly routed through 'lose'.
data OptionalField a = Present a | Absent
  deriving (Show)

acceptUnit :: Validator ()
acceptUnit = Validator (const [])

optionalValidator :: Validator a -> Validator (OptionalField a)
optionalValidator validator = choose routeOptional validator acceptUnit
  where
    routeOptional field = case field of
      Present a -> Left a
      Absent -> Right ()

-- Combined user validator using multiple field types
data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  } deriving (Show)

userValidator :: Validator User
userValidator = divideValidator
  (\user -> (userName user, (userEmail user, userAge user)))
  nameValidator
  (divideValidator id emailValidator ageValidator)
  where
    -- Product validation is Divisible-like: unlike 'choose', it runs both
    -- validators and accumulates their errors.
    divideValidator split (Validator validateB) (Validator validateC) =
      Validator $ \a ->
        let (b, c) = split a
        in validateB b <> validateC c

-- Test cases and main function
main :: IO ()
main = do
  putStrLn "=== Form Validation with Decidable Pattern ==="

  -- Test individual field validation
  putStrLn "\n--- Testing Individual Fields ---"
  let testFields =
        [ Email "user@example.com"
        , Email "invalid-email"
        , Age 25
        , Age 15
        , Age 150
        ]

  mapM_ testField testFields

  -- Test form validation
  putStrLn "\n--- Testing Complete Forms ---"
  currentTime <- getCurrentTime

  let validForm = FormData
        [ Email "alice@company.com"
        , Age 30
        , Email "bob@university.edu"
        , Age 22
        ] currentTime

  let invalidForm = FormData
        [ Email "bad-email"
        , Age 16
        , Email "another@bad"
        , Age 200
        ] currentTime

  testForm validForm
  testForm invalidForm

  -- Test user validation
  putStrLn "\n--- Testing User Validation ---"
  let validUser = User "Alice Smith" "alice@example.com" 25
  let invalidUser = User "X" "bad-email" 15

  testUser validUser
  testUser invalidUser

  -- Test optional field validation
  putStrLn "\n--- Testing Optional Field Validation ---"
  let presentField = Present "test@example.com"
  let absentField = Absent

  testOptionalField presentField
  testOptionalField absentField

  -- Demonstrate the legitimate use of lose with an uninhabited type
  putStrLn "\n--- Impossible Validator (theoretical completeness) ---"
  putStrLn "impossibleValidator is defined but cannot be called with actual input"
  putStrLn "This demonstrates the 'lose' operation for uninhabited types"

testField :: UserField -> IO ()
testField field = do
  let errors = runValidator fieldValidator field
  putStr $ "Field " ++ show field ++ ": "
  if null errors
    then putStrLn "✓ Valid"
    else putStrLn $ "✗ Errors: " ++ show errors

testForm :: FormData -> IO ()
testForm form = do
  let errors = runValidator formValidator form
  putStr $ "Form with " ++ show (length (fields form)) ++ " fields: "
  if null errors
    then putStrLn "✓ All fields valid"
    else putStrLn $ "✗ Validation errors: " ++ show errors

testUser :: User -> IO ()
testUser user = do
  let errors = runValidator userValidator user
  putStr $ "User " ++ userName user ++ ": "
  if null errors
    then putStrLn "✓ Valid"
    else putStrLn $ "✗ Errors: " ++ show errors

testOptionalField :: OptionalField String -> IO ()
testOptionalField field = do
  let emailOptValidator = optionalValidator emailValidator
  let errors = runValidator emailOptValidator field
  putStr $ "Optional field " ++ show field ++ ": "
  if null errors
    then putStrLn "✓ Valid"
    else putStrLn $ "✗ Errors: " ++ show errors
