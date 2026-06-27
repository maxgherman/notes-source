import Control.Monad.State

-- Sum all numbers while keeping track of count
sumWithCount :: [Int] -> State (Int, Int) Int
sumWithCount [] = do
    (total, _) <- get
    return total
sumWithCount (x:xs) = do
    (total, count) <- get
    put (total + x, count + 1)
    sumWithCount xs

-- Usage example
main :: IO ()
main = do
    let numbers = [1,2,3,4,5]
        (result, finalState) = runState (sumWithCount numbers) (0, 0)
    putStrLn $ "Sum: " ++ show result
    putStrLn $ "Final state (sum, count): " ++ show finalState
    -- Output: Sum: 15, Final state: (15, 5)
