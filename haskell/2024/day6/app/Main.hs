{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Data.List
import qualified Data.Map as M

data Grid = Grid
    { gridData :: [Char]
    , gridWidth :: Int
    , gridHeight :: Int
    , gridInitial :: Pos
    , gridCurrent :: Pos
    , gridVisited :: M.Map (Int,Int) (Char, Dir)
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

    let grid = Grid {..}
    printGrid grid
    runPart1 grid
    runPart2 grid


runPart1 :: Grid -> IO ()
runPart1 grid = do
    (g@Grid{..}, completed) <- walkGrid grid
    let visitCount = countVisited g
    if completed then do
        putStrLn ""
        putStrLn $ "Visted: " <> show visitCount <> " tiles"
        putStrLn ""
    else
        putStrLn "Failed to complete, due to a seemingly infinite loop"

    putStrLn ""
    printGrid (stampedGrid g gridVisited)


runPart2 :: Grid -> IO ()
runPart2 grid = do
    -- Get the visited tiles for the vanilla grid, to seed possible roadblocks
    (Grid{..}, _) <- walkGrid grid
    let (Pos initialRow initialCol _) = gridInitial
    let roadBlocks = M.keys (M.delete (initialRow, initialCol) gridVisited)
    let grids = fmap (addRoadBlock grid) roadBlocks
    putStrLn $ "Number of roadblock candidates: " <> show (length grids)
    --completed <- traverse (uncurry walkAndReport) (zip [0..] grids)
    completed <- mapConcurrently (uncurry walkAndReport) (zip roadBlocks grids)
    putStrLn $ "Detected loops: " <> show (length (filter not completed))

    where
        walkAndReport :: (Int,Int) -> Grid -> IO Bool
        walkAndReport blkCoords g = do
            (_, completed) <- walkGrid g
            putStrLn $ show blkCoords <> if completed then " completed" else " LOOP DETECTED"
            pure completed


walkGrid :: Grid -> IO (Grid, Bool)
walkGrid grid = go grid 0
    where
        go :: Grid -> Int -> IO (Grid, Bool)
        go g@Grid{..} revisitStreak =
            if revisitStreak > 100 then
                pure (g, False)
            else do
                let next = nextPos g
                --putStrLn $ "Next Pos: " <> show next
                case next of
                    Nothing -> do
                        let c = markChar gridCurrent gridCurrent
                        let visited = insertVisit gridCurrent c gridVisited
                        let g' = Grid gridData gridWidth gridHeight gridInitial gridCurrent visited
                        pure (g', True)
                    Just p -> do
                        let revisit = isRevisit p gridVisited
                        let g' = moveToPos g p
                        go g' (if revisit then revisitStreak + 1 else 0)


insertVisit :: Pos -> Char -> M.Map (Int,Int) (Char, Dir) -> M.Map (Int,Int) (Char, Dir)
insertVisit (Pos row col dir) c = M.insert (row,col) (c,dir)

isRevisit :: Pos -> M.Map (Int,Int) (Char,Dir) -> Bool
isRevisit (Pos row col _) = M.member (row,col)


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
        isDirChar c = c `elem` ['^', 'v', '>', '<']

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
    let p@(Pos _ _ dir) = gridCurrent
    go dir p 0
    where
        go :: Dir -> Pos -> Int -> Maybe Pos
        go faceDir currPos@(Pos r c _) attempts =
            if attempts > 3 then
                Nothing   -- Walled in by roadblocks on all 4 sides?
            else
                let (p@(Pos r1 c1 _), nextDir) = case faceDir of
                                       UP -> (Pos (r-1) c faceDir, RIGHT)
                                       DOWN -> (Pos (r+1) c faceDir, LEFT)
                                       LEFT -> (Pos r (c-1) faceDir, UP)
                                       RIGHT -> (Pos r (c+1) faceDir, DOWN)
                in
                    if not (isOnGrid grid p) then
                        Nothing
                    else if isBlocked grid p then do
                        go nextDir currPos (attempts+1)
                    else
                        Just $ Pos r1 c1 faceDir

moveToPos :: Grid -> Pos -> Grid
moveToPos Grid{..} pos = do
    let c = if gridCurrent == gridInitial then initialPosToChar gridInitial else markChar gridCurrent pos
    let visited = insertVisit gridCurrent c gridVisited
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

initialPosToChar :: Pos -> Char
initialPosToChar (Pos _ _ dir) =
    case dir of
        UP -> '^'
        DOWN -> 'v'
        LEFT -> '<'
        RIGHT -> '>'

-- Modify the actual grid data. Used to set new roadblocks
addRoadBlock :: Grid -> (Int,Int) -> Grid
addRoadBlock grid@Grid{..} (row,col) =
    let offset = index grid (Pos row col UP)
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

stampedGrid :: Grid -> M.Map (Int,Int) (Char,Dir) -> Grid
stampedGrid = M.foldrWithKey go
    where
        go :: (Int,Int) -> (Char,Dir) -> Grid -> Grid
        go coords markdir grid' = stampGrid grid' coords markdir

        stampGrid :: Grid -> (Int,Int) -> (Char,Dir) -> Grid
        stampGrid g@Grid{..} (row,col) (c,dir) = do
            let offset = index g (Pos row col dir)
            let gridData' = take offset gridData ++ [c] ++ drop (offset+1) gridData
            Grid gridData' gridWidth gridHeight gridInitial gridCurrent gridVisited


