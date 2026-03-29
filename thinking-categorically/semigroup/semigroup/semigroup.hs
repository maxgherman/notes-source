import Prelude hiding (Semigroup, (<>))

-- The Semigroup type class in Haskell
class Semigroup a where
  (<>) :: a -> a -> a

-- List concatenation semigroup
instance Semigroup [a] where
  (<>) = (++)

-- Integer addition semigroup (using Sum wrapper)
newtype Sum a = Sum a deriving (Show)
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

-- Integer multiplication semigroup (using Product wrapper)
newtype Product a = Product a deriving (Show)
instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

-- Examples in action:
main :: IO ()
main = do
  -- String concatenation
  print $ "Hello" <> " " <> "World"  -- "Hello World"

  -- List concatenation
  print $ ([1,2] :: [Int]) <> [3,4] <> [5,6]    -- [1,2,3,4,5,6]

  -- Sum semigroup
  print $ (Sum 1 :: Sum Integer) <> Sum 2 <> Sum 3    -- Sum 6

  -- Product semigroup
  print $ (Product 2 :: Product Integer) <> Product 3 <> Product 4  -- Product 24
