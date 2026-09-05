module Main where

pairFunc :: (String, Int) -> Bool
pairFunc (str, n) = length str > n

curriedFunc :: Int -> String -> Bool
curriedFunc n str = length str > n

productToExponential :: ((a, b) -> c) -> b -> a -> c
productToExponential f b a = f (a, b)

exponentialToProduct :: (b -> a -> c) -> (a, b) -> c
exponentialToProduct f (a, b) = f b a

pairUnit :: b -> (a -> (a, b))
pairUnit b = \a -> (a, b)

pairCounit :: (a, (a -> b)) -> b
pairCounit (a, f) = f a

main :: IO ()
main = do
  print $ pairFunc ("adjunction", 5)
  print $ productToExponential pairFunc 5 "adjunction"
  print $ exponentialToProduct curriedFunc ("cat", 5)
  print $ pairCounit (3 :: Int, (* 2))
