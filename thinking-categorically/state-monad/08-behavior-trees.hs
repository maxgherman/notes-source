import Control.Monad.State

-- AI state for game NPCs
data AIState = AIState
  { aiHealth :: Int
  , aiPosition :: (Int, Int)
  , aiTarget :: Maybe (Int, Int)
  , aiCooldown :: Int
  } deriving (Show)

-- Individual AI behaviors using State monad
-- Each behavior returns Bool: True = success, False = failure

-- 1. Target acquisition behavior
findTarget :: State AIState Bool
findTarget = do
    state' <- get
    -- Simplified target finding logic - in a real game, this would
    -- search for nearby enemies, check line of sight, etc.
    put $ state' { aiTarget = Just (10, 10) }
    return True  -- Always succeeds in this simple example

-- 2. Movement behavior
moveToTarget :: State AIState Bool
moveToTarget = do
    state' <- get
    case aiTarget state' of
        Nothing -> return False  -- No target to move to
        Just (tx, ty) -> do
            let (x, y) = aiPosition state'
                -- Simple pathfinding: move one step closer to target
                newX
                  | x < tx = x + 1
                  | x > tx = x - 1
                  | otherwise = x
                newY
                  | y < ty = y + 1
                  | y > ty = y - 1
                  | otherwise = y
            put $ state' { aiPosition = (newX, newY) }
            return True  -- Movement succeeded

-- 3. Combat behavior
attackIfInRange :: State AIState Bool
attackIfInRange = do
    state' <- get
    case aiTarget state' of
        Nothing -> return False  -- No target to attack
        Just target -> do
            let distance = calculateDistance (aiPosition state') target
            -- Check if target is in range AND attack is off cooldown
            if distance <= 2 && aiCooldown state' == 0
                then do
                    put $ state' { aiCooldown = 3 }  -- Set attack cooldown
                    return True   -- Attack succeeded
                else return False -- Either out of range or on cooldown

-- 4. Composite behavior: Complete AI decision tree
aiTurn :: State AIState String
aiTurn = do
    -- Step 1: Try to find a target
    hasTarget <- findTarget
    if hasTarget
        then do
            -- Step 2: Try to attack if possible
            attacked <- attackIfInRange
            if attacked
                then return "Attacked!"
                else do
                    -- Step 3: If can't attack, try to move closer
                    moved <- moveToTarget
                    return $ if moved then "Moved toward target" else "Standing still"
        else return "Looking for target"

-- Helper function
calculateDistance :: (Int, Int) -> (Int, Int) -> Int
calculateDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Usage example
main :: IO ()
main = do
    let initialState = AIState 100 (0, 0) Nothing 0
        (action, finalState) = runState aiTurn initialState
    putStrLn $ "AI Action: " ++ action
    putStrLn $ "Final AI State: " ++ show finalState
