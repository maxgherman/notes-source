import Control.Monad

-- Kleisli arrows for safe division
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Partially applied Kleisli arrows
divideBy2 :: Double -> Maybe Double
divideBy2 x = safeDivide x 2

reciprocal :: Double -> Maybe Double
reciprocal x = safeDivide 1 x

divideBy4 :: Double -> Maybe Double
divideBy4 x = safeDivide x 4

-- Kleisli composition using (<=<)
complexOperation :: Double -> Maybe Double
complexOperation = divideBy4 <=< reciprocal <=< divideBy2

-- Alternative using (>=>)
complexOperation' :: Double -> Maybe Double
complexOperation' = divideBy2 >=> reciprocal >=> divideBy4

-- Usage
main :: IO ()
main = do
    print $ complexOperation 2.0   -- Just 0.25
    print $ complexOperation 0.0   -- Nothing
    print $ complexOperation' 2.0  -- Just 0.25
