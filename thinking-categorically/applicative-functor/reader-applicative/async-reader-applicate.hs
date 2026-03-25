import Control.Concurrent.Async (Concurrently(..))

-- Run two computations in parallel, then combine their results
parallelAdd :: IO Int
parallelAdd =
  runConcurrently $
    (+) <$> Concurrently (return 3)
        <*> Concurrently (return 4)
