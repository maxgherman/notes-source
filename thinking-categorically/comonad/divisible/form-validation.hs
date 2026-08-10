import Data.Functor.Contravariant (Contravariant (contramap))

-- Contravariant equivalent of Applicative: divide the input between two
-- consumers, or accept it without performing any work.
class Contravariant f => Divisible f where
  conquer :: f a
  divide :: (a -> (b, c)) -> f b -> f c -> f a

-- A validator consumes an input and returns all errors it finds.
newtype Validator e a = Validator { runValidator :: a -> [e] }

instance Contravariant (Validator e) where
  contramap f (Validator validate) = Validator (validate . f)

instance Divisible (Validator e) where
  conquer = Validator (const [])

  divide split (Validator validateB) (Validator validateC) =
    Validator $ \a ->
      let (b, c) = split a
      in validateB b <> validateC c

data User = User
  { userEmail :: String
  , userAge :: Int
  } deriving (Show)

validateEmail :: Validator String String
validateEmail = Validator $ \email ->
  ["Invalid email" | '@' `notElem` email]

validateAge :: Validator String Int
validateAge = Validator $ \age ->
  ["Must be 18+" | age < 18]

-- Usage: Validate multiple fields simultaneously
validateUser :: Validator String User
validateUser = divide
  (\user -> (userEmail user, userAge user))
  validateEmail
  validateAge

-- Validate just one field by adapting an existing validator.
validateUserEmail :: Validator String User
validateUserEmail = contramap userEmail validateEmail

-- A Divisible consumer can choose to accept every input.
acceptAnyUser :: Validator String User
acceptAnyUser = conquer

displayValidation :: String -> Validator String a -> a -> IO ()
displayValidation label validator value =
  putStrLn $ label ++ ": " ++ case runValidator validator value of
    [] -> "valid"
    errors -> unwords errors

-- Test cases
main :: IO ()
main = do
  let validUser = User "alice@example.com" 30
      invalidEmail = User "alice.example.com" 30
      invalidAge = User "alice@example.com" 16
      invalidUser = User "alice.example.com" 16

  putStrLn "Individual field validators:"
  displayValidation "email" validateEmail "alice.example.com"
  displayValidation "age" validateAge 16

  putStrLn "\ncontramap projects a User to the field being validated:"
  displayValidation "email only" validateUserEmail invalidEmail

  putStrLn "\ndivide validates both fields and accumulates errors:"
  displayValidation (show validUser) validateUser validUser
  displayValidation (show invalidEmail) validateUser invalidEmail
  displayValidation (show invalidAge) validateUser invalidAge
  displayValidation (show invalidUser) validateUser invalidUser

  putStrLn "\nconquer accepts without validation:"
  displayValidation (show invalidUser) acceptAnyUser invalidUser
