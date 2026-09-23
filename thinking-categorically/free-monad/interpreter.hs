{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.State.Strict (State, get, put, runState)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)

data Free f a
  = Pure a                    -- Pure value (return)
  | Free (f (Free f a))       -- Suspended computation
instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free layer) = Free (fmap (fmap g) layer)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g <*> x = fmap g x
  Free layer <*> x = Free (fmap (<*> x) layer)

instance Functor f => Monad (Free f) where
  Pure a >>= k = k a
  Free layer >>= k = Free (fmap (>>= k) layer)

-- Our basic console operations
data ConsoleF next
  = WriteLine String next           -- Write a line to console
  | ReadLine (String -> next)       -- Read a line from console
  deriving (Functor)

-- Type alias for our Free Monad
type Console = Free ConsoleF
-- Smart constructors for our DSL
writeLine :: String -> Console ()
writeLine s = liftF (WriteLine s ())

readLine :: Console String
readLine = liftF (ReadLine id)

-- Helper function to lift functors into Free Monads
liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- A simple greeting program
greetingProgram :: Console ()
greetingProgram = do
  writeLine "Hello! What's your name?"
  name <- readLine
  writeLine ("Nice to meet you, " ++ name ++ "!")
  writeLine "What's your favorite color?"
  color <- readLine
  writeLine (name ++ " likes " ++ color ++ ". Great choice!")

-- A more complex program with logic
surveyProgram :: Console ()
surveyProgram = do
  writeLine "Welcome to our survey!"
  writeLine "Are you over 18? (yes/no)"
  age <- readLine
  if age == "yes"
    then do
      writeLine "What's your occupation?"
      job <- readLine
      writeLine ("Thank you! We have: occupation = " ++ job)
    else writeLine "Thanks for your interest, but this survey is for adults only."

-- Interpret to actual IO operations
runConsoleIO :: Console a -> IO a
runConsoleIO (Pure a) = return a
runConsoleIO (Free (WriteLine s next)) = do
  putStrLn s
  runConsoleIO next
runConsoleIO (Free (ReadLine f)) = do
  input <- getLine
  runConsoleIO (f input)

-- Interpret with predefined inputs for testing
runConsoleTest :: [String] -> Console a -> (a, [String], [String])
runConsoleTest inputs program =
  let ((result, outputs), remainingInputs) =
        runState (runWriterT (interpret program)) inputs
  in (result, outputs, remainingInputs)
  where
    interpret :: Console a -> WriterT [String] (State [String]) a
    interpret (Pure a) = return a
    interpret (Free (WriteLine s next)) = do
      tell [s]  -- Record output
      interpret next
    interpret (Free (ReadLine f)) = do
      remaining <- get
      case remaining of
        [] -> error "No more test inputs!"
        (x:xs) -> do
          put xs  -- Consume input
          interpret (f x)

-- Simple mock that just collects operations
data MockResult = MockResult
  { mockOutputs :: [String]
  , mockInputsUsed :: [String]
  } deriving (Show, Eq)

runConsoleMock :: [String] -> Console a -> MockResult
runConsoleMock inputs prog = MockResult outputs usedInputs
  where
    (outputs, usedInputs) = runMock inputs prog

    runMock :: [String] -> Console a -> ([String], [String])
    runMock _ (Pure _) = ([], [])
    runMock remaining (Free (WriteLine s next)) =
      let (nextOutputs, used) = runMock remaining next
      in (s : nextOutputs, used)
    runMock [] (Free (ReadLine _)) = error "No mock input available!"
    runMock (i:is) (Free (ReadLine f)) =
      let (nextOutputs, used) = runMock is (f i)
      in (nextOutputs, i : used)

main :: IO ()
main = do
  putStrLn "=== Running with real IO ==="
  runConsoleIO greetingProgram

  putStrLn "\n=== Running with test data ==="
  let testInputs = ["Alice", "blue"]
  let (_, outputs, _) = runConsoleTest testInputs greetingProgram
  putStrLn "Outputs:"
  mapM_ putStrLn outputs

  putStrLn "\n=== Running survey with test data ==="
  let (_, surveyOutputs, _) = runConsoleTest ["yes", "Engineer"] surveyProgram
  mapM_ putStrLn surveyOutputs

  putStrLn "\n=== Running with mock ==="
  let mockInputs = ["Bob", "red"]
  let mockResult = runConsoleMock mockInputs greetingProgram
  print mockResult
