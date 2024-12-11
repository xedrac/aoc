module Main where

--import Data.Array
import Data.List
import Control.Monad (when)

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
    let (Just pos) = position g width
    --print contents
    --putStrLn $ show width <> " x " <> show height
    --print pos
    let grid = Grid g width height pos
    printGrid grid

    walk grid 50000
    where
        walk :: Grid -> Int -> IO ()
        walk grid@(Grid _ _ _ p) iter =
            when (iter > 0) $ do
                case step grid of
                    Nothing -> do
                        let g = setValue grid p 'X'
                        putStrLn ""
                        printGrid g
                        putStrLn ""
                        putStrLn "**** The End ****"
                        putStrLn ""
                        putStrLn $ "Visted: " <> show (countVisited g) <> " tiles"
                    Just g -> do
                        --putStrLn ""
                        --printGrid g
                        walk g (iter - 1)

countVisited :: Grid -> Int
countVisited (Grid g _ _ _) = length $ filter (== 'X') g


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
        isDirChar c = c `elem` ['^', 'v', '>', '<']

index :: Grid -> Pos -> Int
index (Grid _ width _ _) (Pos row col _) = row * width + col

value :: Grid -> Pos -> Char
value grid@(Grid g _ _ _) pos = g !! index grid pos

isBlocked :: Grid -> Pos -> Bool
isBlocked grid pos = value grid pos == '#'

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

-- Given a grid and current position, and next position, return
-- a new grid that reflects the move to the new position
updateGrid :: Grid -> Pos -> Grid
updateGrid grid@(Grid _ _ _ pos0@(Pos row0 col0 _)) pos1@(Pos row1 col1 dir1) =
    if row0 == row1 && col0 == col1 then
        setValue grid pos1 (charFromDir dir1)   -- Only direction changed
    else do
        let grid' = setValue grid pos0 'X'      -- Mark where we were
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

