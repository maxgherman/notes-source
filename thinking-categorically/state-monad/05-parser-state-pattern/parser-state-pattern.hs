import Control.Monad.State
import Data.Maybe (isJust)

-- Parser state containing input and position
data ParseState = ParseState
  { input :: String
  , position :: Int
  } deriving (Show)

-- Parse a specific character
parseChar :: Char -> State ParseState (Maybe Char)
parseChar expected = do
    state' <- get
    case drop (position state') (input state') of
        (c:_) | c == expected -> do
            put $ state' { position = position state' + 1 }
            return $ Just c
        _ -> return Nothing

-- Parse a string by parsing each character
parseString :: String -> State ParseState (Maybe String)
parseString str = do
    results <- mapM parseChar str
    return $ if all isJust results
             then Just str
             else Nothing

-- Parse a number (simplified)
parseNumber :: State ParseState (Maybe Int)
parseNumber = do
    state' <- get
    let remaining = drop (position state') (input state')
        digits = takeWhile (`elem` "0123456789") remaining
    if null digits
        then return Nothing
        else do
            put $ state' { position = position state' + length digits }
            return $ Just (read digits)

-- Usage example
main :: IO ()
main = do
    let initialState = ParseState "hello123world" 0
        (result1, state1) = runState (parseString "hello") initialState
        (result2, state2) = runState parseNumber state1
        (result3, finalState) = runState (parseString "world") state2

    putStrLn $ "Parsed 'hello': " ++ show result1
    putStrLn $ "Parsed number: " ++ show result2
    putStrLn $ "Parsed 'world': " ++ show result3
    putStrLn $ "Final state: " ++ show finalState
    -- Output: Parsed 'hello': Just "hello", Parsed number: Just 123, etc.
