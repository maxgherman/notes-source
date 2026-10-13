{-# LANGUAGE Arrows #-}
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- State machine Arrow
newtype StateMachine s a b = StateMachine
  { runStateMachine :: s -> a -> (b, s) }

instance Category (StateMachine s) where
  id = StateMachine $ \s a -> (a, s)
  StateMachine f . StateMachine g = StateMachine $ \s a ->
    let (b, s')  = g s a
        (c, s'') = f s' b
    in (c, s'')

instance Arrow (StateMachine s) where
  arr f = StateMachine $ \s a -> (f a, s)
  first (StateMachine f) = StateMachine $ \s (a, c) ->
    let (b, s') = f s a
    in ((b, c), s')

instance ArrowChoice (StateMachine s) where
  left (StateMachine f) = StateMachine $ \s input -> case input of
    Left a -> let (b, s') = f s a in (Left b, s')
    Right c -> (Right c, s)
  right (StateMachine f) = StateMachine $ \s input -> case input of
    Left a -> (Left a, s)
    Right c -> let (b, s') = f s c in (Right b, s')

-- State machine primitives
getState :: StateMachine s a s
getState = StateMachine $ \s _ -> (s, s)

setState :: StateMachine s s ()
setState = StateMachine $ \_ s -> ((), s)

modifyState :: (s -> s) -> StateMachine s a ()
modifyState f = StateMachine $ \s _ -> ((), f s)

-- Traffic light state machine
data LightState = Red | Yellow | Green deriving (Show, Eq)
data LightInput = TimerTick | EmergencyStop deriving (Show, Eq)

trafficLight :: StateMachine LightState LightInput LightState
trafficLight = proc input -> do
  currentState <- getState -< ()
  newState <- arr transition -< (currentState, input)
  setState -< newState
  returnA -< newState
  where
    transition (Red, TimerTick) = Green
    transition (Green, TimerTick) = Yellow
    transition (Yellow, TimerTick) = Red
    transition (_, EmergencyStop) = Red

-- Counter with reset functionality
counterMachine :: StateMachine Int String Int
counterMachine = proc command -> do
  count <- getState -< ()
  case command of
    "inc" -> do
      modifyState (+1) -< ()
      getState -< ()
    "dec" -> do
      modifyState (subtract 1) -< ()
      getState -< ()
    "reset" -> do
      setState -< 0
      returnA -< 0
    _ -> returnA -< count

-- Working example functions
main :: IO ()
main = do
  putStrLn "Testing counter machine:"
  print $ runStateMachine counterMachine 0 "inc"    -- (1, 1)
  print $ runStateMachine counterMachine 5 "dec"    -- (4, 4)
  print $ runStateMachine counterMachine 10 "reset" -- (0, 0)
  putStrLn "Testing traffic light:"
  print $ runStateMachine trafficLight Red TimerTick      -- (Green, Green)
  print $ runStateMachine trafficLight Green TimerTick    -- (Yellow, Yellow)
  print $ runStateMachine trafficLight Yellow EmergencyStop -- (Red, Red)

-- Example usage:
-- runStateMachine counterMachine 0 "inc" evaluates to (1, 1)
-- runStateMachine trafficLight Red TimerTick evaluates to (Green, Green)
