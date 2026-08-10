{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import Data.Void
import Data.Functor.Contravariant hiding (Predicate)

-- Decidable typeclass definition
class Contravariant f => Decidable f where
  lose :: (a -> Void) -> f a
  choose :: (a -> Either b c) -> f b -> f c -> f a

-- Predicate that can handle impossible cases
newtype Predicate a = Predicate (a -> Bool)

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

instance Decidable Predicate where
  lose f = Predicate (absurd . f)  -- impossible input

  choose split (Predicate pb) (Predicate pc) =
    Predicate $ \a -> case split a of
      Left b -> pb b
      Right c -> pc c

-- Usage: Route different types of input to appropriate handlers
data Input = TextInput String | NumberInput Int

-- Predicates for specific input types
isLongText :: Predicate String
isLongText = Predicate ((>= 10) . length)

isEvenNumber :: Predicate Int
isEvenNumber = Predicate even

-- Combined predicate that routes based on input type
isValidInput :: Predicate Input
isValidInput = choose inputSplit isLongText isEvenNumber
  where
    inputSplit = \case
      TextInput s -> Left s
      NumberInput n -> Right n

-- Another routing example using a different sum type
data Shape = Circle Double | Rectangle Double Double

-- Separate predicates for different shape properties
isLargeCircle :: Predicate Double
isLargeCircle = Predicate (> 10)

isWideRectangle :: Predicate (Double, Double)
isWideRectangle = Predicate (\(w, h) -> w > h * 2)

-- Route shapes to appropriate predicates
shapeValidator :: Predicate Shape
shapeValidator = choose shapeRoute isLargeCircle isWideRectangle
  where
    shapeRoute shape = case shape of
      Circle r -> Left r
      Rectangle w h -> Right (w, h)

-- Example demonstrating 'lose' with a truly impossible type
data Impossible  -- uninhabited type (no constructors)

-- This predicate handles the impossible case
impossiblePredicate :: Predicate Impossible
impossiblePredicate = lose (\case {})  -- empty case analysis proves impossibility

-- Attach an impossible branch to a real predicate. A value of type Void
-- cannot be constructed, so only the Left branch can occur.
leftOnlyValidator :: Predicate a -> Predicate (Either a Void)
leftOnlyValidator validator = choose id validator (lose id)

-- Test cases
main :: IO ()
main = do
  let (Predicate test) = isValidInput
  print $ test (TextInput "Hello World!")  -- True (long text)
  print $ test (TextInput "Hi")            -- False (short text)
  print $ test (NumberInput 42)            -- True (even number)
  print $ test (NumberInput 13)            -- False (odd number)

  -- Test shape validation
  let (Predicate shapeTest) = shapeValidator
  print $ shapeTest (Circle 15.0)          -- True (large circle)
  print $ shapeTest (Circle 5.0)           -- False (small circle)
  print $ shapeTest (Rectangle 20.0 5.0)   -- True (wide rectangle)
  print $ shapeTest (Rectangle 10.0 10.0)  -- False (square)

  -- Demonstrate 'lose' with impossible predicate
  putStrLn "Impossible predicate defined for theoretical completeness"
  -- Note: impossiblePredicate can never be called with actual input

  -- Demonstrate the left identity law for choose/lose
  let (Predicate leftOnlyTest) = leftOnlyValidator isEvenNumber
  print $ leftOnlyTest (Left 42)         -- True (even number)
  print $ leftOnlyTest (Left 13)         -- False (odd number)
  -- There is no total way to construct a value of Right Void.
