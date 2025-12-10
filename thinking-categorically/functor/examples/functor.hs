data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

tree :: Tree Int
tree = Node (Leaf 1) (Leaf 2)

main :: IO ()
main = do
  print $ fmap (+1) [1, 2, 3]
  print $ fmap (+1) (Just 5)     
  print $ fmap (+1) Nothing
  print $ fmap (*2) tree
  print $ fmap (++ " World") (Just "Hello")
  print $ fmap (++ " World") Nothing