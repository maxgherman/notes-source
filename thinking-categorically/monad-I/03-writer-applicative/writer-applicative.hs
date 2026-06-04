data Writer w a = Writer a w deriving (Show)

-- Writer is a Functor
instance Functor (Writer w) where
    fmap f (Writer a w) = Writer (f a) w

-- Writer is an Applicative (requires Monoid constraint for log type)
instance Monoid w => Applicative (Writer w) where
    pure a = Writer a mempty
    Writer f w1 <*> Writer a w2 = Writer (f a) (w1 <> w2)

-- Helper function to create logged values
logged :: a -> [String] -> Writer [String] a
logged value logs = Writer value logs

-- Example functions that produce logged results
addWithLog :: Int -> Writer [String] Int
addWithLog x = logged x ["Added " ++ show x]

multiplyWithLog :: Int -> Writer [String] Int
multiplyWithLog x = logged (x * 2) ["Multiplied " ++ show x ++ " by 2"]

-- Pure applicative computation
main :: IO ()
main = do
    let w1 = addWithLog 3
        w2 = addWithLog 5

        -- Using applicative operators to combine logged computations
        result = (+) <$> w1 <*> w2

        -- More complex applicative combination
        complexResult = (\a b c -> a + b * c) <$> addWithLog 10 <*> multiplyWithLog 2 <*> addWithLog 3

    case result of
        Writer value logs -> do
            putStrLn $ "Simple result: " ++ show value  -- 8
            putStrLn $ "Logs: " ++ show logs             -- ["Added 3", "Added 5"]

    case complexResult of
        Writer value logs -> do
            putStrLn $ "Complex result: " ++ show value  -- 24 (10 + 4 * 3)
            putStrLn $ "All logs: " ++ show logs         -- ["Added 10", "Multiplied 2 by 2", "Added 3"]
