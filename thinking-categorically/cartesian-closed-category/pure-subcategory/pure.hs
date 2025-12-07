{-# LANGUAGE ExplicitForAll #-}

-- Terminal Object: Unit type ()
terminal :: ()
terminal = ()

-- Every type has exactly one morphism to the terminal object
toTerminal :: forall a. a -> ()
toTerminal _ = ()

-- Binary Products: Tuples (A, B)
type Product a b = (a, b)

-- Product construction and projections
makePair :: a -> b -> Product a b
makePair x y = (x, y)

firstProj :: Product a b -> a
firstProj (x, _) = x

secondProj :: Product a b -> b
secondProj (_, y) = y

-- Universal property of products: pairing morphism
pairing :: (x -> a) -> (x -> b) -> (x -> Product a b)
pairing f g = \x -> (f x, g x)

-- Exponential Objects: Function types A -> B
type Exponential a b = a -> b

-- Evaluation morphism
eval :: (Exponential a b, a) -> b
eval (f, x) = f x

-- Currying / Uncurrying
curry' :: (Product a b -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (Product a b -> c)
uncurry' f = \(x, y) -> f x y

-- Composition (demonstrates exponential structure)
compose :: Exponential b c -> Exponential a b -> Exponential a c
compose f g x = f (g x)

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

add5and7 :: Int -> Int
add5and7 = addThree 5 7

pipeline :: Int -> String
pipeline = show . (* 2) . (+ 1)

main :: IO ()
main = do
  putStrLn "=== Cartesian Closed Category Basic Constructions ==="
  putStrLn $ "terminal: " ++ show terminal
  putStrLn $ "toTerminal 42: " ++ show (toTerminal (42 :: Int))
  -- Explicit type annotations below avoid GHC -Wtype-defaults warnings
  let p :: (Int, Char)
      p = makePair (3 :: Int) 'a'
  putStrLn $ "makePair 3 'a': " ++ show p
  putStrLn $ "firstProj p: " ++ show (firstProj p)
  putStrLn $ "secondProj p: " ++ show (secondProj p)
  putStrLn $ "pairing (+1) (*2) 5: " ++ show (pairing ((+1) :: Int -> Int) ((*2) :: Int -> Int) (5 :: Int))
  putStrLn $ "eval ((+10),5): " ++ show (eval ((+10), 5 :: Int))
  putStrLn $ "curry'/uncurry' round trip (multiply): " ++
    let multiply :: (Int, Int) -> Int
        multiply (x,y) = x * y
        r = uncurry' (curry' multiply) (6 :: Int, 7 :: Int)
    in show r
  putStrLn $ "compose show (*2) 7: " ++ compose (show :: Int -> String) ((*2) :: Int -> Int) (7 :: Int)
  putStrLn $ "addThree 1 2 3: " ++ show (addThree (1 :: Int) (2 :: Int) (3 :: Int))
  putStrLn $ "add5and7 9: " ++ show (add5and7 (9 :: Int))
  putStrLn $ "pipeline 5: " ++ pipeline (5 :: Int)
