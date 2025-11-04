-- Self-hom-set Hom(Int, Int) contains:
import Data.Char (toUpper)
increment :: Int -> Int
increment x = x + 1

double :: Int -> Int
double x = x * 2

negate' :: Int -> Int
negate' x = -x

identity :: Int -> Int
identity x = x  -- This is id_Int

-- Composition within the self-hom-set
doubleIncrement :: Int -> Int
doubleIncrement = double . increment  -- Still Int -> Int

-- String transformations - Hom(String, String)
reverse' :: String -> String
reverse' = reverse

uppercase :: String -> String
uppercase = map toUpper

addExclamation :: String -> String
addExclamation s = s ++ "!"

-- Composition
shout :: String -> String
shout = addExclamation . uppercase . reverse'