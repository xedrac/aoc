module Main where

import Data.List
--import Control.Monad (when)

data Grid = Grid [Char] Int Int Pos
    deriving (Show, Eq)

data Pos = Pos Int Int Dir
    deriving (Show, Eq)

data Dir = UP | DOWN | LEFT | RIGHT
    deriving (Show, Eq)

main :: IO ()
main = do
    contents <- lines <$> readFile "input"
    let width = length $ head contents
    let g = concat contents
    let height = length g `div` width
    let pos = case position g width of
                 Nothing -> error "doh"
                 Just p -> p
    --print contents
    --putStrLn $ show width <> " x " <> show height
    --print pos
    let grid0 = Grid g width height pos
    printGrid grid0

    let (grid', visited, completed) = walk grid0
    if completed then do
        putStrLn ""
        printGrid grid'
        putStrLn ""
        putStrLn $ "Visted: " <> show (countVisited grid') <> " tiles in " <> show (length visited) <> " steps"
        putStrLn ""
    else
        putStrLn "Failed to complete before iter max hit"


    -- Try all of the visited tiles as roadblocks (minus the starting position)
    -- and see which ones got stuck in an infinite loop
    let candidates = nub visited
    putStrLn $ "Candidates for RoadBlocks: " <> show (length candidates)

    numInfLoopBlocks <- filter not <$> traverse (exitsWithCustomRoadBlock grid0) (zip [0..] candidates)
    putStrLn $ "numInfLoopBlocks: " <> show (length numInfLoopBlocks)


-- Add a roadblock to the grid at position pos, then walk
-- the grid like normal.  Except this time, terminate early
-- if we step on N previously traversed tiles in a row.  This
-- should cut down on the amount of work we have to do to detect
-- infinite loops
exitsWithCustomRoadBlock :: Grid -> (Int, Pos) -> IO Bool
exitsWithCustomRoadBlock grid (idx, roadBlockPos) = do
    let grid' = addRoadBlock grid roadBlockPos
    let res = go grid' 0
    putStrLn $ show idx <> ": " <> show res
    pure res
    where
        go :: Grid -> Int -> Bool
        go g streak =
            streak < 10 && case nextPos g of
                Nothing -> True
                Just p ->
                    let maybeG = step g
                    in
                        case maybeG of
                            Nothing -> True
                            Just g' ->
                                if isVisited g p then
                                    go g' (streak + 1)
                                else
                                    go g' 0



---- Add a roadblock and walk the grid.  Returns whether or not
---- it completed the walk with the new roadblock in place
--completesWithRoadBlock :: Grid -> Pos -> Bool
--completesWithRoadBlock grid roadBlockPos = do
--    let grid' = addRoadBlock grid roadBlockPos
--    let (_, _, completed) = walk grid'
--    completed



-- Add a custom roadblock at pos
addRoadBlock :: Grid -> Pos -> Grid
addRoadBlock grid pos = setValue grid pos 'O'


walk :: Grid -> (Grid, [Pos], Bool)
walk grid = go grid 10000 []
    where
        go :: Grid -> Int -> [Pos] -> (Grid, [Pos], Bool)
        go grid'@(Grid _ _ _ p) iter visited = do
            if iter > 0 then
                case step grid' of
                    Nothing -> do
                        let g = setValue grid p (markChar p p)
                        (g, visited, True)
                    Just g -> do
                        --putStrLn ""
                        --printGrid g
                        let (Grid _ _ _ p') = g
                        go g (iter - 1) (p':visited)
            else
                (grid', visited, False)


countVisited :: Grid -> Int
countVisited (Grid g _ _ _) = length $ filter (\c -> c == '|' || c == '-' || c == '+') g


charFromDir :: Dir -> Char
charFromDir dir = case dir of
    UP -> '^'
    RIGHT -> '>'
    DOWN -> 'v'
    LEFT -> '<'

dirFromChar :: Char -> Maybe Dir
dirFromChar c = do
    case c of
         '^' -> Just UP
         '>' -> Just RIGHT
         'v' -> Just DOWN
         '<' -> Just LEFT
         _ -> Nothing

-- Given initial grid contents, find the initial position/direction by looking through the cells
position :: [Char] -> Int -> Maybe Pos
position g width = do
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
index (Grid _ width _ _) (Pos row col _) = row * width + col

value :: Grid -> Pos -> Char
value grid@(Grid g _ _ _) pos = g !! index grid pos

isBlocked :: Grid -> Pos -> Bool
isBlocked grid pos =
    let v = value grid pos
    in
        v == '#' || v == 'O'

isVisited :: Grid -> Pos -> Bool
isVisited grid pos =
    let v = value grid pos
    in
        v == '|' || v == '-' || v == '+'

isOnGrid :: Grid -> Bool
isOnGrid (Grid _ width height (Pos row col _)) = row >= 0 && row < height && col >= 0 && col < width

turn :: Pos -> Pos
turn (Pos row col dir) =
    let pos' = Pos row col
    in
        case dir of
             UP -> pos' RIGHT
             RIGHT -> pos' DOWN
             DOWN -> pos' LEFT
             LEFT -> pos' UP

nextPos :: Grid -> Maybe Pos
nextPos grid@(Grid g width height pos@(Pos row col dir)) = do
    let p = case dir of
                UP -> Pos (row-1) col dir
                DOWN -> Pos (row+1) col dir
                LEFT -> Pos row (col-1) dir
                RIGHT -> Pos row (col+1) dir
    if not (isOnGrid (Grid g width height p)) then
        Nothing
    else if isBlocked grid p then
        Just $ turn pos
    else
        Just p

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

-- Given a grid and current position, and next position, return
-- a new grid that reflects the move to the new position
updateGrid :: Grid -> Pos -> Grid
updateGrid grid@(Grid _ _ _ pos0@(Pos row0 col0 _)) pos1@(Pos row1 col1 dir1) =
    if row0 == row1 && col0 == col1 then
        setValue grid pos1 (charFromDir dir1)   -- Only direction changed
    else do
        let grid' = setValue grid pos0 (markChar pos0 pos1) -- Mark where we were
        setValue grid' pos1 (charFromDir dir1)  -- Mark where we are now

-- Given a Grid and a Pos, set the Pos' value to Char
setValue :: Grid -> Pos -> Char -> Grid
setValue grid@(Grid g width height _) pos c =
    let offset = index grid pos
        newgrid = take offset g ++ [c] ++ drop (offset+1) g
    in
        Grid newgrid width height pos

-- Move a step from current position by finding the next position,
-- and updating the grid accordingly
step :: Grid -> Maybe Grid
step grid = do
    let pos' = nextPos grid
    case pos' of
        Just p -> Just (updateGrid grid p)
        Nothing -> Nothing

printGrid :: Grid -> IO ()
printGrid (Grid g width height _) = do
    let rowidxs = [0 .. (height-1)]
    mapM_ (printRow g width) rowidxs
    where
        printRow grid w rowidx = print $ take w (drop (rowidx * w) grid)

