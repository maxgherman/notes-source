import Control.Monad.State

-- ID generator state
newtype IdGen = IdGen { nextId :: Int } deriving (Show)

-- Generate a single unique ID
generateId :: State IdGen Int
generateId = do
    IdGen current <- get
    put $ IdGen (current + 1)
    return current

-- Generate multiple IDs
generateIds :: Int -> State IdGen [Int]
generateIds 0 = return []
generateIds n = do
    firstId <- generateId
    restIds <- generateIds (n - 1)
    return (firstId : restIds)

-- Usage example
main :: IO ()
main = do
    let initialGen = IdGen 1
        (ids, finalGen) = runState (generateIds 5) initialGen
    putStrLn $ "Generated IDs: " ++ show ids
    putStrLn $ "Final generator state: " ++ show finalGen
    -- Output: Generated IDs: [1,2,3,4,5], Final generator state: IdGen {nextId = 6}
