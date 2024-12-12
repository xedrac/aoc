{-# LANGUAGE RecordWildCards #-}

module Main where

--import Control.Parallel.Strategies (parMap, rpar)
import Control.Concurrent.Async (mapConcurrently)
import Data.List
import qualified Data.Map as M

data Grid = Grid
    { gridData :: [Char]
    , gridWidth :: Int
    , gridHeight :: Int
    , gridInitial :: Pos
    , gridCurrent :: Pos
    , gridVisited :: M.Map Pos Char
    }
    deriving (Show, Eq)

data Pos = Pos Int Int Dir
    deriving (Show, Eq, Ord)

data Dir = UP | DOWN | LEFT | RIGHT
    deriving (Show, Eq, Ord)

main :: IO ()
main = do
    contents <- lines <$> readFile "input"
    let gridWidth = length $ head contents
    let gridData = concat contents
    let gridHeight = length gridData `div` gridWidth
    let gridInitial = case findInitialPosition gridData gridWidth of
                          Nothing -> error "doh"
                          Just p -> p
    let gridCurrent = gridInitial
    let gridVisited = M.empty

    --print contents
    --putStrLn $ show gridWidth <> " x " <> show gridHeight
    --print pos
    let grid = Grid {..}
    printGrid grid
    runPart1 grid
    runPart2 grid


runPart1 :: Grid -> IO ()
runPart1 grid = do
    let (g, completed) = walkGrid grid
    let visitCount = countVisited g
    if completed then do
        putStrLn ""
        printGrid g
        putStrLn ""
        putStrLn $ "Visted: " <> show visitCount <> " tiles"
        putStrLn ""
    else
        putStrLn "Failed to complete, due to a seemingly infinite loop"


--parallelTraverse :: NFData a => (a -> b) -> [a] -> [b]
--parallelTraverse = runEval $ parTraversable rpar
--parallelTraverse f xs = parEval $ parTraversable rpar (f) xs

runPart2 :: Grid -> IO ()
runPart2 grid = do
    -- Get the visited tiles for the vanilla grid, to seed possible roadblocks
    let (Grid{..}, _) = walkGrid grid
    let roadBlocks = M.keys (M.delete gridInitial gridVisited)
    let grids = fmap (addRoadBlock grid) roadBlocks
    putStrLn $ "Number of roadblack candidates: " <> show (length grids)
    --completed <- traverse (uncurry walkAndReport) (zip [0..] grids)
    --completed <- sequence $ parMap rpar (uncurry walkAndReport) (zip [0..] grids)
    completed <- mapConcurrently (uncurry walkAndReport) (zip [0..] grids)
    putStrLn $ "Detected loops: " <> show (length (filter not completed))

    where
        walkAndReport :: Int -> Grid -> IO Bool
        walkAndReport idx g = do
            let (_, completed) = walkGrid g
            putStrLn $ show idx <> if completed then " completed" else " LOOP DETECTED"
            pure completed


walkGrid :: Grid -> (Grid, Bool)
walkGrid grid = go grid 0
    where
        go :: Grid -> Int -> (Grid, Bool)
        go g@Grid{..} revisitStreak =
            if revisitStreak > 100 then
                (g, False)
            else do
                case nextPos g of
                    Nothing -> (g, True)
                    Just p -> do
                        let revisit = M.member p gridVisited
                        let g' = moveToPos g p
                        go g' (if revisit then revisitStreak + 1 else 0)


countVisited :: Grid -> Int
countVisited Grid{..} = M.size gridVisited


-- Given initial grid data, find the initial position/direction by looking through the cells
findInitialPosition :: [Char] -> Int -> Maybe Pos
findInitialPosition g width = do
    idx <- findIndex isDirChar g
    let row = idx `div` width
        col = idx - (row * width)
        val = g !! idx
        pos = Pos row col
    case val of
         '^' -> Just (pos UP)
         '>' -> Just (pos RIGHT)
         'v' -> Just (pos DOWN)
         '<' -> Just (pos LEFT)
         _ -> Nothing
    where
        isDirChar c = c `elem` ['^', 'v', '>', '<', '/']

index :: Grid -> Pos -> Int
index Grid{..} (Pos row col _) = row * gridWidth + col

value :: Grid -> Pos -> Char
value g@Grid{..} pos = gridData !! index g pos

isBlocked :: Grid -> Pos -> Bool
isBlocked grid pos =
    let c = value grid pos
    in
        c == '#' || c == 'O'

isVisited :: Grid -> Pos -> Bool
isVisited grid pos =
    let c = value grid pos
    in
        c == '|' || c == '-' || c == '+'

isOnGrid :: Grid -> Pos -> Bool
isOnGrid Grid{..} (Pos row col _) = row >= 0 && row < gridHeight && col >= 0 && col < gridWidth


nextPos :: Grid -> Maybe Pos
nextPos grid@Grid{..} = do
    let (Pos row col dir) = gridCurrent
    let p1 = case dir of
                UP -> Pos (row-1) col dir
                DOWN -> Pos (row+1) col dir
                LEFT -> Pos row (col-1) dir
                RIGHT -> Pos row (col+1) dir
    if not (isOnGrid grid p1) then
        Nothing
    else if isBlocked grid p1 then do
        -- If blocked, turn right and take the next block there
        let p2 = case dir of
                     UP -> Pos row (col+1) RIGHT
                     DOWN -> Pos row (col-1) LEFT
                     LEFT -> Pos (row+1) col UP
                     RIGHT -> Pos (row-1) col DOWN
        if not (isOnGrid grid p2) then
            Nothing
        else   -- TODO:  Don't assume you can't have two roadblocks in a row
            Just p2
    else
        Just p1


-- Take a step and update the grid accordingly.
stepOnce :: Grid -> Maybe Grid
stepOnce grid = do
    let mp = nextPos grid
    case mp of
        Nothing -> Nothing
        Just p -> Just $ moveToPos grid p

    --let (Pos row col dir) = gridCurrent
    --let p1 = case dir of
    --            UP -> Pos (row-1) col dir
    --            DOWN -> Pos (row+1) col dir
    --            LEFT -> Pos row (col-1) dir
    --            RIGHT -> Pos row (col+1) dir
    --if not (isOnGrid grid p1) then
    --    Nothing
    --else if isBlocked grid p1 then do
    --    -- If blocked, turn right and take the next block there
    --    let p2 = case dir of
    --                 UP -> Pos row (col+1) RIGHT
    --                 DOWN -> Pos row (col-1) LEFT
    --                 LEFT -> Pos (row+1) col UP
    --                 RIGHT -> Pos (row-1) col DOWN
    --    if not (isOnGrid grid p2) then
    --        Nothing
    --    else
    --        Just $ moveToPos p2
    --else
    --    Just $ moveToPos p1


moveToPos :: Grid -> Pos -> Grid
moveToPos Grid{..} pos = do
    let c = markChar gridCurrent pos
    let visited = M.insert pos c gridVisited
    Grid gridData gridWidth gridHeight gridInitial pos visited

-- Given two positions, determine which character
-- to represent having traveled there based on the direction
markChar :: Pos -> Pos -> Char
markChar (Pos _ _ dir0) (Pos _ _ dir1) =
    case (dir0, dir1) of
        (UP, UP) -> '|'
        (DOWN, DOWN) -> '|'
        (LEFT, LEFT) -> '-'
        (RIGHT, RIGHT) -> '-'
        (UP, RIGHT) -> '+'
        (RIGHT, DOWN) -> '+'
        (DOWN, LEFT) -> '+'
        (LEFT, UP) -> '+'
        _ -> '?'

-- Modify the actual Grid, used to set new roadblocks
addRoadBlock :: Grid -> Pos -> Grid
addRoadBlock grid@Grid{..} pos =
    let offset = index grid pos
        grid' = take offset gridData ++ ['O'] ++ drop (offset+1) gridData
    in
        Grid grid' gridWidth gridHeight gridInitial gridCurrent gridVisited


printGrid :: Grid -> IO ()
printGrid Grid{..} = do
    let rowidxs = [0 .. (gridHeight-1)]
    mapM_ printRow rowidxs
    where
        printRow :: Int -> IO ()
        printRow rowidx = print $ take gridWidth (drop (rowidx * gridWidth) gridData)

