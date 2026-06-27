import Control.Monad.State
import qualified Data.Set as Set

-- Application configuration state
data AppConfig = AppConfig
  { apiUrl :: String
  , timeout :: Int
  , features :: Set.Set String
  } deriving (Show)

-- Enable a feature if not already enabled
enableFeature :: String -> State AppConfig Bool
enableFeature feature = do
    config <- get
    if Set.member feature (features config)
        then return False  -- Already enabled
        else do
            put $ config { features = Set.insert feature (features config) }
            return True

-- Configure multiple features
configureApp :: State AppConfig String
configureApp = do
    darkMode <- enableFeature "darkMode"
    notifications <- enableFeature "notifications"
    analytics <- enableFeature "analytics"
    return $ "Configured: darkMode=" ++ show darkMode ++
             ", notifications=" ++ show notifications ++
             ", analytics=" ++ show analytics

-- Usage example
main :: IO ()
main = do
    let initialConfig = AppConfig "https://api.example.com" 5000 Set.empty
        (result, finalConfig) = runState configureApp initialConfig
    putStrLn result
    putStrLn $ "Final config: " ++ show finalConfig
    -- Output shows which features were newly enabled
