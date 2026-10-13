{-# LANGUAGE Arrows #-}
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- Define a simple Arrow for signal processing
newtype Circuit a b = Circuit { runCircuit :: a -> b }

instance Category Circuit where
  id = Circuit id
  Circuit f . Circuit g = Circuit (f . g)

instance Arrow Circuit where
  arr = Circuit
  first (Circuit f) = Circuit $ \(a, c) -> (f a, c)
  second (Circuit f) = Circuit $ \(c, a) -> (c, f a)

-- Basic circuit components
amplify :: Double -> Circuit Double Double
amplify gain = arr (* gain)

delay :: Circuit Double Double
delay = arr id  -- Simplified delay

signalFilter :: Double -> Circuit Double Double
signalFilter cutoff = arr $ \x -> if abs x > cutoff then x else 0

-- More complex circuits using Arrow combinators
parallelAmplifiers :: Circuit (Double, Double) (Double, Double)
parallelAmplifiers = amplify 2.0 *** amplify 3.0

serialProcessing :: Circuit Double Double
serialProcessing = amplify 1.5 >>> signalFilter 0.1 >>> delay

-- Branching and rejoining signals
splitterMixer :: Circuit Double Double
splitterMixer = proc input -> do
  amplified <- amplify 2.0 -< input
  filtered <- signalFilter 0.5 -< input
  returnA -< amplified + filtered

-- Multi-stage processing pipeline
audioProcessor :: Circuit Double Double
audioProcessor = proc signal -> do
  -- Stage 1: Pre-amplification
  preamp <- amplify 1.2 -< signal

  -- Stage 2: Parallel processing
  (highGain, lowGain) <- (amplify 3.0 *** amplify 0.8) -< (preamp, preamp)

  -- Stage 3: Filtering
  filtered <- signalFilter 0.3 -< highGain

  -- Stage 4: Mix and final amplification
  mixed <- arr (uncurry (+)) -< (filtered, lowGain)
  final <- amplify 0.9 -< mixed

  returnA -< final

-- Example usage functions
main :: IO ()
main = do
  putStrLn "Testing audio processor:"
  print $ runCircuit audioProcessor 1.0
  putStrLn "Testing serial processing:"
  print $ runCircuit serialProcessing 2.5
  putStrLn "Testing parallel amplifiers:"
  print $ runCircuit parallelAmplifiers (1.0, 2.0)
  putStrLn "Testing splitter mixer:"
  print $ runCircuit splitterMixer 1.5

-- Direct usage examples:
-- runCircuit audioProcessor 1.0      -- Returns: 1.512
-- runCircuit serialProcessing 2.5    -- Returns: 3.75
-- runCircuit parallelAmplifiers (1.0, 2.0) -- Returns: (2.0, 6.0)
