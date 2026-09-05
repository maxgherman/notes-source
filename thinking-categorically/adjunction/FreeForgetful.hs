module Main where

import Data.Monoid (Sum(..))

type FreeMonoid a = [a]

freeMap :: (a -> b) -> FreeMonoid a -> FreeMonoid b
freeMap = map

unit :: a -> FreeMonoid a
unit x = [x]

counit :: Monoid m => FreeMonoid m -> m
counit = mconcat

newtype MonoidHom a m = MonoidHom
  { applyHom :: a -> m
  }

extend :: Monoid m => (a -> m) -> MonoidHom (FreeMonoid a) m
extend interpret = MonoidHom (counit . freeMap interpret)

restrict :: MonoidHom (FreeMonoid a) m -> a -> m
restrict hom = applyHom hom . unit

data Symbol = A | B | Space
  deriving (Eq, Show)

weight :: Symbol -> Sum Int
weight A     = Sum 1
weight B     = Sum 2
weight Space = Sum 0

score :: MonoidHom (FreeMonoid Symbol) (Sum Int)
score = extend weight

word :: FreeMonoid Symbol
word = [A, B, B, A]

exampleScore :: Sum Int
exampleScore = applyHom score word

renderSymbol :: Symbol -> String
renderSymbol A     = "a"
renderSymbol B     = "b"
renderSymbol Space = " "

render :: MonoidHom (FreeMonoid Symbol) String
render = extend renderSymbol

exampleText :: String
exampleText = applyHom render [A, B, Space, B, A]

preservesIdentity :: Bool
preservesIdentity = applyHom score [] == mempty

preservesComposition :: Bool
preservesComposition =
  applyHom score ([A, B] <> [B, A])
    == applyHom score [A, B] <> applyHom score [B, A]

roundTripOnGenerator :: Bool
roundTripOnGenerator = restrict (extend weight) B == weight B

main :: IO ()
main = do
  putStrLn $ "score: " ++ show (getSum exampleScore)
  putStrLn $ "rendered: " ++ exampleText
  putStrLn $ "preserves identity: " ++ show preservesIdentity
  putStrLn $ "preserves composition: " ++ show preservesComposition
  putStrLn $ "round trip on generator: " ++ show roundTripOnGenerator
