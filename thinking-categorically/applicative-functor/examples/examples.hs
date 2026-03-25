import Data.Functor.Identity

-- Values
result :: Identity Int
result = Identity 1

-- fmap chaining: fmap (+1) then fmap show with concatenation
mapped :: Identity String
mapped = fmap (\x -> show x ++ " + 1") (fmap (+1) result)
-- mapped == Identity "2 + 1"

-- Applicative apply: Identity (x -> x + 1) <*> Identity 1
applied :: Identity Int
applied = Identity (+1) <*> result
-- applied == Identity 2
