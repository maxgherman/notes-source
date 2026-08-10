{-# LANGUAGE DeriveFunctor #-}
import Control.Comonad
import Text.Printf (printf)

-- 2D grid with focus on current cell
data Grid a = Grid
  { gridData :: [[a]]
  , focusX :: Int
  , focusY :: Int
  , gridWidth :: Int
  , gridHeight :: Int
  } deriving (Show, Functor)

-- Create a grid from a 2D list
mkGrid :: [[a]] -> Grid a
mkGrid [] = error "Cannot create empty grid"
mkGrid rows@(r:_)
  | null r = error "Cannot create a grid with empty rows"
  | any ((/= width) . length) rows = error "Grid rows must have equal lengths"
  | otherwise = Grid rows 0 0 width (length rows)
  where
    width = length r

-- Create a grid filled with a value
fillGrid :: Int -> Int -> a -> Grid a
fillGrid w h val
  | w <= 0 || h <= 0 = error "Grid dimensions must be positive"
  | otherwise = Grid (replicate h (replicate w val)) 0 0 w h

-- Safe grid access with bounds checking
safeGet :: Grid a -> Int -> Int -> Maybe a
safeGet (Grid rows _ _ w h) x y
  | x >= 0 && x < w && y >= 0 && y < h = Just $ (rows !! y) !! x
  | otherwise = Nothing

-- Get grid dimensions
gridDimensions :: Grid a -> (Int, Int)
gridDimensions (Grid _ _ _ w h) = (w, h)

-- Move focus to specific coordinates
moveTo :: Int -> Int -> Grid a -> Grid a
moveTo x y grid@(Grid _ _ _ w h)
  | x >= 0 && x < w && y >= 0 && y < h = grid { focusX = x, focusY = y }
  | otherwise = grid

-- Move focus in cardinal directions
moveUp, moveDown, moveLeft, moveRight :: Grid a -> Grid a
moveUp grid@(Grid _ x y _ _) = moveTo x (y - 1) grid
moveDown grid@(Grid _ x y _ _) = moveTo x (y + 1) grid
moveLeft grid@(Grid _ x y _ _) = moveTo (x - 1) y grid
moveRight grid@(Grid _ x y _ _) = moveTo (x + 1) y grid

instance Comonad Grid where
  extract (Grid rows x y _ _) = (rows !! y) !! x

  duplicate grid@(Grid _ _ _ w h) = Grid
    { gridData = [[ moveTo x y grid
                  | x <- [0..w-1]]
                  | y <- [0..h-1]]
    , focusX = focusX grid
    , focusY = focusY grid
    , gridWidth = w
    , gridHeight = h
    }

  extend f grid@(Grid _ _ _ w h) = Grid
    { gridData = [[ f (moveTo x y grid)
                  | x <- [0..w-1]]
                  | y <- [0..h-1]]
    , focusX = focusX grid
    , focusY = focusY grid
    , gridWidth = w
    , gridHeight = h
    }

-- Get all 8 neighbors (Moore neighborhood) including the center
getNeighbors :: Grid a -> [a]
getNeighbors grid@(Grid _ x y _ _) =
  [ value
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , Just value <- [safeGet grid (x+dx) (y+dy)]
  ]

-- Get 4-connected neighbors (Von Neumann neighborhood)
getNeighbors4 :: Grid a -> [a]
getNeighbors4 grid@(Grid _ x y _ _) =
  [ value
  | (dx, dy) <- [(0,1), (1,0), (0,-1), (-1,0)]
  , Just value <- [safeGet grid (x+dx) (y+dy)]
  ]

-- Count living neighbors for Conway's Game of Life
countLivingNeighbors :: Grid Bool -> Int
countLivingNeighbors grid@(Grid _ x y _ _) =
  length $ filter id
    [ alive
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , not (dx == 0 && dy == 0)  -- exclude center cell
    , Just alive <- [safeGet grid (x+dx) (y+dy)]
    ]

-- Conway's Game of Life rules
gameOfLifeRule :: Grid Bool -> Bool
gameOfLifeRule grid =
  let current = extract grid
      neighbors = countLivingNeighbors grid
  in case (current, neighbors) of
    (True, 2)  -> True   -- survival with 2 neighbors
    (True, 3)  -> True   -- survival with 3 neighbors
    (False, 3) -> True   -- birth with 3 neighbors
    _          -> False  -- death in all other cases

-- Apply Game of Life rules to entire grid
nextGeneration :: Grid Bool -> Grid Bool
nextGeneration = extend gameOfLifeRule

-- Image processing: blur filter
blur :: Grid Int -> Int
blur grid =
  let neighbors = getNeighbors grid
  in if null neighbors
     then extract grid
     else sum neighbors `div` length neighbors

-- Apply blur filter to entire grid
blurImage :: Grid Int -> Grid Int
blurImage = extend blur

-- Edge detection using simple gradient
edgeDetect :: Grid Int -> Int
edgeDetect grid =
  let current = extract grid
      neighbors = getNeighbors4 grid
      differences = map (abs . (current -)) neighbors
  in if null differences
     then 0
     else maximum differences

-- Apply edge detection to entire grid
detectEdges :: Grid Int -> Grid Int
detectEdges = extend edgeDetect

-- Utility: Convert grid to string for display
showGrid :: Show a => Grid a -> String
showGrid (Grid rows _ _ _ _) =
  unlines $ map (unwords . map show) rows

-- Conway patterns
glider :: Grid Bool
glider = mkGrid
  [ [False, True,  False, False, False]
  , [False, False, True,  False, False]
  , [True,  True,  True,  False, False]
  , [False, False, False, False, False]
  , [False, False, False, False, False]
  ]

blinker :: Grid Bool
blinker = mkGrid
  [ [False, False, False, False, False]
  , [False, False, True,  False, False]
  , [False, False, True,  False, False]
  , [False, False, True,  False, False]
  , [False, False, False, False, False]
  ]

-- Image processing test data
testImage :: Grid Int
testImage = mkGrid
  [ [10, 20, 30, 40, 50]
  , [15, 25, 35, 45, 55]
  , [20, 30, 40, 50, 60]
  , [25, 35, 45, 55, 65]
  , [30, 40, 50, 60, 70]
  ]

-- Display functions for different cell types
displayLife :: Grid Bool -> String
displayLife (Grid rows _ _ _ _) =
  unlines $ map (map (\b -> if b then '#' else '.')) rows

displayImage :: Grid Int -> String
displayImage (Grid rows _ _ _ _) =
  unlines $ map (unwords . map (printf "%2d")) rows

-- Run Conway's Game of Life simulation
runLifeSimulation :: Int -> Grid Bool -> [Grid Bool]
runLifeSimulation 0 grid = [grid]
runLifeSimulation n grid =
  grid : runLifeSimulation (n-1) (nextGeneration grid)

displayIndexedLife :: (Int, Grid Bool) -> IO ()
displayIndexedLife (generation, grid) = do
  putStrLn $ "Generation " ++ show generation ++ ":"
  putStrLn $ displayLife grid

-- Main demonstration
main :: IO ()
main = do
  putStrLn "=== Haskell Comonad: 2D Grid Computing ==="
  putStrLn "Demonstrating spatial context and neighborhood operations\n"

  -- Conway's Game of Life
  putStrLn "=== Conway's Game of Life ==="
  putStrLn "\n--- Glider Pattern Evolution ---"
  let gliderGenerations = runLifeSimulation 4 glider
  mapM_ displayIndexedLife (zip [0..] gliderGenerations)

  putStrLn "\n--- Blinker Pattern Evolution ---"
  let blinkerGenerations = runLifeSimulation 3 blinker
  mapM_ displayIndexedLife (zip [0..] blinkerGenerations)

  -- Image processing
  putStrLn "\n=== Image Processing with Comonad Grid ==="
  putStrLn "\n--- Original Image ---"
  putStrLn $ displayImage testImage

  putStrLn "\n--- Blurred Image ---"
  let blurred = blurImage testImage
  putStrLn $ displayImage blurred

  putStrLn "\n--- Edge Detection ---"
  let edges = detectEdges testImage
  putStrLn $ displayImage edges

  -- Demonstrate comonad operations
  putStrLn "\n=== Demonstrating Comonad Operations ==="

  let grid :: Grid Int
      grid = mkGrid [[1,2,3], [4,5,6], [7,8,9]]
  putStrLn "\nOriginal 3x3 grid:"
  putStrLn $ showGrid grid
  putStrLn $ "Dimensions: " ++ show (gridDimensions grid)

  putStrLn "Create a filled 4x2 grid:"
  let filled :: Grid Int
      filled = fillGrid 4 2 7
  putStrLn $ showGrid filled

  putStrLn "Map over the grid with its Functor instance:"
  putStrLn $ showGrid (fmap (* 10) grid)

  putStrLn "\nFocus on center (1,1) - extract value:"
  let centered = moveTo 1 1 grid
  print $ extract centered

  putStrLn "\nNeighbors of center cell:"
  print $ getNeighbors centered

  putStrLn "\nValues reached by moving from the center:"
  print
    [ ("up", extract $ moveUp centered)
    , ("down", extract $ moveDown centered)
    , ("left", extract $ moveLeft centered)
    , ("right", extract $ moveRight centered)
    ]

  putStrLn "\nSafe access inside and outside the grid:"
  print (safeGet grid 2 2, safeGet grid 3 3)

  putStrLn "\nApply sum function to all positions (extend):"
  let sumGrid = extend (sum . getNeighbors) grid
  putStrLn $ showGrid sumGrid

  -- Demonstrate duplicate operation
  putStrLn "\n=== Demonstrating Duplicate Operation ==="
  let smallGrid :: Grid Int
      smallGrid = mkGrid [[1,2], [3,4]]
  putStrLn "\nSmall 2x2 grid:"
  putStrLn $ showGrid smallGrid

  putStrLn "\nDuplicated grid (grid of grids at each position):"
  let duplicated = duplicate smallGrid
  putStrLn "Each cell now contains a grid focused at that position"
  putStrLn $ "Focus (0,0): " ++ show (extract $ extract $ moveTo 0 0 duplicated)
  putStrLn $ "Focus (1,1): " ++ show (extract $ extract $ moveTo 1 1 duplicated)

  -- Advanced: Custom cellular automaton
  putStrLn "\n=== Custom Cellular Automaton: Majority Rule ==="
  let majorityRule :: Grid Bool -> Bool
      majorityRule neighborhood =
        let neighbors = getNeighbors neighborhood
            trueCount = length $ filter id neighbors
            totalCount = length neighbors
        in trueCount > totalCount `div` 2

  let customPattern = mkGrid
        [ [True,  False, True,  False, True ]
        , [False, True,  False, True,  False]
        , [True,  False, True,  False, True ]
        , [False, True,  False, True,  False]
        , [True,  False, True,  False, True ]
        ]

  putStrLn "\nInitial pattern:"
  putStrLn $ displayLife customPattern

  putStrLn "\nAfter majority rule:"
  let afterMajority = extend majorityRule customPattern
  putStrLn $ displayLife afterMajority

  -- Demonstrate extend composition
  putStrLn "\n=== Comonad Law Demonstration ==="
  putStrLn "Law: extend extract = id"
  let testGrid :: Grid Int
      testGrid = mkGrid [[10, 20], [30, 40]]
  let extendExtract = extend extract testGrid
  putStrLn $ "Original: " ++ showGrid testGrid
  putStrLn $ "extend extract: " ++ showGrid extendExtract
  putStrLn $ "Equal: " ++ show (gridData testGrid == gridData extendExtract)

  putStrLn "\nLaw: extract . extend f = f (for any focused grid)"
  let focused = moveTo 0 0 testGrid
  let f = sum . getNeighbors
  let extractExtendF = extract $ extend f focused
  let directF = f focused
  putStrLn $ "extract (extend f) grid: " ++ show extractExtendF
  putStrLn $ "f grid: " ++ show directF
  putStrLn $ "Equal: " ++ show (extractExtendF == directF)
