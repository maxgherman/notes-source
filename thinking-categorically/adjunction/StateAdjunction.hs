module Main where

newtype Product s a = Product (s, a)
newtype Reader s a = Reader (s -> a)
newtype State s a = State (s -> (a, s))

toState :: Reader s (Product s a) -> State s a
toState (Reader f) = State $ \s ->
  let Product (s', a) = f s
  in (a, s')

fromState :: State s a -> Reader s (Product s a)
fromState (State f) = Reader $ \s ->
  let (a, s') = f s
  in Product (s', a)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State g = State $ \s ->
    let (fab, s') = f s
        (a, s'') = g s'
    in (fab a, s'')

instance Monad (State s) where
  State g >>= f = State $ \s ->
    let (a, s') = g s
        State h = f a
    in h s'

counter :: State Int String
counter = do
  n <- get
  put (n + 1)
  pure ("Count: " ++ show n)

runState :: State s a -> s -> (a, s)
runState (State f) = f

main :: IO ()
main = do
  let (message, finalCount) = runState counter 0
  putStrLn message
  putStrLn $ "Final count: " ++ show finalCount
