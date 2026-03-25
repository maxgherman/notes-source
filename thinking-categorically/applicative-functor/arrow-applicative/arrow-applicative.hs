instance Applicative ((->) r) where
  pure x = \_ -> x
  f <*> g = \x -> f x (g x)

f :: Int -> String
f = show

g :: Int -> Bool
g = even

combinedApplicative :: Int -> String
combinedApplicative = (++) <$> (\x -> show (x + 1)) <*> (\x -> "-" ++ show (x * 2))
