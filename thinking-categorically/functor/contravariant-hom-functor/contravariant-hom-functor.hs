-- Contravariant Hom functor: (- -> B) for fixed B
-- This is captured by the Contravariant typeclass

import Data.Functor.Contravariant

-- For fixed target type B = String, we have Hom(-, String):
newtype ToString a = ToString (a -> String)

instance Contravariant ToString where
  contramap f (ToString g) = ToString (g . f)

-- Examples of functions targeting String
showInt :: ToString Int
showInt = ToString show

showLength :: ToString String
showLength = ToString (show . length)

-- Contravariant mapping: if we have f: A -> B and g: B -> String,
-- we can create g . f: A -> String
main :: IO ()
main = do
  let ToString intToStr = showInt
  let ToString strToStr = contramap (show :: Int -> String) showLength

  print $ intToStr 42        -- "42"
  print $ strToStr 42        -- "2" (length of "42")
