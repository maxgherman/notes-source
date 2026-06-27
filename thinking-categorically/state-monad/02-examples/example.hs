import Control.Monad.State

-- Game state type
data GameState = GameState
  { health :: Int
  , score :: Int
  , inventory :: [String]
  , position :: (Int, Int)
  } deriving (Show)

-- Initial game state
initialState :: GameState
initialState = GameState 100 0 [] (0, 0)

-- State operations using the State monad
collectItem :: String -> State GameState String
collectItem item = do
    gameState <- get
    put $ gameState { inventory = item : inventory gameState
                    , score = score gameState + 10 }
    return $ "Collected " ++ item

takeDamage :: Int -> State GameState String
takeDamage damage = do
    gameState <- get
    let newHealth = max 0 (health gameState - damage)
    put $ gameState { health = newHealth }
    return $ if newHealth > 0 then "Still alive" else "Game over"

movePlayer :: Int -> Int -> State GameState String
movePlayer dx dy = do
    gameState <- get
    let (x, y) = position gameState
        newPos = (x + dx, y + dy)
    put $ gameState { position = newPos }
    return $ "Moved to " ++ show newPos

-- Monadic composition - state is automatically threaded
playGame :: State GameState [String]
playGame = do
    msg1 <- collectItem "sword"
    msg2 <- takeDamage 20
    msg3 <- movePlayer 5 3
    return [msg1, msg2, msg3]

-- Run the game
main :: IO ()
main = do
    let (messages, finalState) = runState playGame initialState
    mapM_ putStrLn messages
    putStrLn $ "Final state: " ++ show finalState
