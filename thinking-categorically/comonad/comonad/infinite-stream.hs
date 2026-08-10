{-# LANGUAGE DeriveFunctor #-}
import Control.Comonad

-- An infinite stream. For a tape, the head of each side is the value
-- immediately adjacent to the focus.
data Stream a = a :< Stream a
  deriving Functor

data Tape a = Tape (Stream a) a (Stream a)
  deriving Functor

repeatS :: a -> Stream a
repeatS x = x :< repeatS x

iterateS :: (a -> a) -> a -> Stream a
iterateS f x = f x :< iterateS f (f x)

takeS :: Int -> Stream a -> [a]
takeS n _ | n <= 0 = []
takeS n (x :< xs) = x : takeS (n-1) xs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l :< ls) focus rs) = Tape ls l (focus :< rs)

moveRight :: Tape a -> Tape a
moveRight (Tape ls focus (r :< rs)) = Tape (focus :< ls) r rs

instance Comonad Tape where
  extract (Tape _ focus _) = focus

  duplicate tape = Tape
    (iterateS moveLeft tape)
    tape
    (iterateS moveRight tape)

  extend f = fmap f . duplicate

-- A finite seed embedded in an infinite background. The first seed value is
-- focused; values outside the seed are the supplied background value.
fromListWithDefault :: a -> [a] -> Tape a
fromListWithDefault background [] =
  Tape (repeatS background) background (repeatS background)
fromListWithDefault background (x:xs) =
  Tape (repeatS background) x (appendDefault xs)
  where
    appendDefault [] = repeatS background
    appendDefault (y:ys) = y :< appendDefault ys

getNeighbors :: Tape a -> (a, a, a)
getNeighbors (Tape (left :< _) current (right :< _)) =
  (left, current, right)

-- Rule 30: left XOR (center OR right)
rule30 :: Tape Bool -> Bool
rule30 tape =
  let (left, current, right) = getNeighbors tape
  in left /= (current || right)

-- Apply rule to entire stream (next generation)
nextGeneration :: (Tape Bool -> Bool) -> Tape Bool -> Tape Bool
nextGeneration rule = extend rule

initialPattern :: Tape Bool
initialPattern = fromListWithDefault False [True]

-- Run simulation for n steps
runSimulation :: Int -> (Tape Bool -> Bool) -> Tape Bool -> [Tape Bool]
runSimulation 0 _ stream = [stream]
runSimulation n rule stream =
  stream : runSimulation (n-1) rule (nextGeneration rule stream)

displayGeneration :: Int -> Tape Bool -> String
displayGeneration radius (Tape left focus right) =
  map displayCell $
    reverse (takeS radius left) ++ [focus] ++ takeS radius right
  where
    displayCell alive = if alive then '#' else '.'

main :: IO ()
main = do
  putStrLn "1D cellular automaton using a comonadic tape"
  let generations = runSimulation 10 rule30 initialPattern
  mapM_ (putStrLn . displayGeneration 15) generations
