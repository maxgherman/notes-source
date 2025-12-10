class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  first :: (a -> c) -> f a b -> f c b
  second :: (b -> d) -> f a b -> f a d

  -- Default implementations
  first f = bimap f id
  second g = bimap id g

-- Tuple/Pair bifunctor - the most basic example
instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)

-- Either bifunctor - for sum types
instance Bifunctor Either where
  bimap f _ (Left x)  = Left (f x)
  bimap _ g (Right y) = Right (g y)

-- Examples using tuples
pairExample :: IO ()
pairExample = do
  let pair = (5, "hello")

  -- Transform both elements
  let both = bimap (*2) length pair          -- (10, 5)
  print both

  -- Transform only first element
  let firstOnly = first (*3) pair            -- (15, "hello")
  print firstOnly

  -- Transform only second element
  let secondOnly = second (++ "!") pair      -- (5, "hello!")
  print secondOnly

-- Examples using Either
eitherExample :: IO ()
eitherExample = do
  let leftValue = Left 42 :: Either Int String
  let rightValue = Right "world" :: Either Int String

  -- Transform both sides (but only the present one is affected)
  let leftResult = bimap (*2) length leftValue    -- Left 84
  let rightResult = bimap (*2) length rightValue  -- Right 5

  print leftResult   -- Left 84
  print rightResult  -- Right 5

-- Custom bifunctor: Tree with two different value types
data BiTree a b = BiLeaf a b | BiNode (BiTree a b) (BiTree a b)
  deriving (Show)

instance Bifunctor BiTree where
  bimap f g (BiLeaf x y) = BiLeaf (f x) (g y)
  bimap f g (BiNode l r) = BiNode (bimap f g l) (bimap f g r)

treeExample :: IO ()
treeExample = do
  let tree = BiNode (BiLeaf 1 "a") (BiLeaf 2 "b")
  let transformed = bimap (*10) length tree
  print transformed  -- BiNode (BiLeaf 10 1) (BiLeaf 20 1)

main :: IO ()
main = do
    pairExample
    eitherExample
    treeExample
