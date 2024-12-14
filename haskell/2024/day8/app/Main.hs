{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.List as L
import qualified Data.Map as M


newtype SignalMap = SignalMap (M.Map Char [Pos])
    deriving (Show)

data Grid = Grid
    { gridData :: [Char]
    , gridWidth :: Int
    , gridHeight :: Int
    } deriving (Show)

data Pos = Pos Int Int
    deriving (Show, Eq)

data SignalPair = SignalPair Pos Pos Char
    deriving (Show)


main :: IO ()
main = do
    grid <- loadGrid "input"
    let sm@(SignalMap _sm') = signalMap grid
    let pairs = signalPairs sm

    printGrid grid
    putStrLn ""
    part1 grid pairs

    putStrLn ""
    part2 grid pairs

    --testAntiNodes

    putStrLn ""


part1 :: Grid -> [[SignalPair]] -> IO ()
part1 grid allPairs = do
    let allAnti = fmap signalAntinodes allPairs
    let uniqueAnti = L.nub (concat allAnti)
    --print uniqueAnti
    printGridWithAnti grid uniqueAnti
    putStrLn $ "Part1: " <> show (length uniqueAnti)
    where
        signalAntinodes :: [SignalPair] -> [Pos]
        signalAntinodes pairs = L.nub $ concatMap (antiNodes grid) pairs

part2 :: Grid -> [[SignalPair]] -> IO ()
part2 grid allPairs = do
    let allHarmonics = fmap signalHarmonics allPairs
    let uniqueHarmonics = L.nub (concat allHarmonics)
    printGridWithAnti grid uniqueHarmonics

    let Grid{..} = updateGridWithAntis grid uniqueHarmonics
    let active = filter (/= '.') gridData
    putStrLn $ "Part2: " <> show (length active)
    where
        signalHarmonics :: [SignalPair] -> [Pos]
        signalHarmonics pairs = L.nub $ concatMap (harmonics grid) pairs


updateGrid :: Grid -> (Char, Pos) -> Grid
updateGrid grid@Grid{..} (c, Pos row col) = do
    let offset = row * gridWidth + col
    if offset < 0 || offset >= length gridData then
        grid
    else do
        let currChar = gridData !! offset
        let newChar = if currChar == '.' then c else '%'
        let newData = L.take offset gridData ++ [newChar] ++ L.drop (offset+1) gridData
        Grid newData gridWidth gridHeight

updateGridWithAntis :: Grid -> [Pos] -> Grid
updateGridWithAntis grid antis = do
    let chars = replicate (length antis) '#'
    foldl updateGrid grid (zip chars antis)

printGridWithAnti :: Grid -> [Pos] -> IO ()
printGridWithAnti grid antis = do
    let newGrid = updateGridWithAntis grid antis
    printGrid newGrid

printGrid :: Grid -> IO ()
printGrid Grid{..} = go gridData
    where
        go dat =
            if null dat then
                pure ()
            else do
                putStrLn $ take gridWidth dat
                go (drop gridWidth dat)



signalPairs :: SignalMap -> [[SignalPair]]
signalPairs (SignalMap sm) = do
    let xs = M.toList sm
    fmap getSignalPairs xs
    where
        getSignalPairs :: (Char, [Pos]) -> [SignalPair]
        getSignalPairs (c, positions) = uniqueSignalPairs c positions


offsetToPos :: Grid -> Int -> Pos
offsetToPos Grid{..} offset =
    let row = offset `div` gridWidth
        col = offset - (row * gridWidth)
    in
        Pos row col

-- Build a map of signal characters to positions on the grid
signalMap :: Grid -> SignalMap
signalMap g@Grid{..} = do
    let allPos = fmap (offsetToPos g) [0..(length gridData-1)]
    let charPos = zip gridData allPos
    L.foldl storeSignal (SignalMap M.empty) charPos
    where
        storeSignal :: SignalMap -> (Char, Pos) -> SignalMap
        storeSignal (SignalMap sm) (c, pos) = SignalMap $ if c == '.' then sm else M.insertWith (++) c [pos] sm


isValidPos :: Grid -> Pos -> Bool
isValidPos Grid{..} (Pos row col) = row >= 0 && row < gridHeight && col >= 0 && col < gridWidth


distance :: SignalPair -> (Int, Int)
distance (SignalPair (Pos r0 c0) (Pos r1 c1) _) = (r1 - r0, c1 - c0)

loadGrid :: FilePath -> IO Grid
loadGrid file = do
    lines' <- lines <$> readFile file
    let gridWidth = length $ head lines'
    let gridData = concat lines'
    let gridHeight = length gridData `div` gridWidth
    pure Grid{..}


uniqueSignalPairs :: Char -> [Pos] -> [SignalPair]
uniqueSignalPairs c positions = [SignalPair p0 p1 c | (p0:px) <- L.tails positions, p1 <- px]


testAntiNodes :: IO ()
testAntiNodes = do
    grid <- loadGrid "input.test"
    printGrid grid
    let sm@(SignalMap _sm') = signalMap grid
    print sm
    let pairs = signalPairs sm
    print pairs

-- Given a signal pair, calculate the antinode positions for that pair
antiNodes :: Grid -> SignalPair -> [Pos]
antiNodes grid s@(SignalPair (Pos r0 c0) (Pos r1 c1) _) = do
    let (dr,dc) = distance s
    let a0 = Pos (r0 - dr) (c0 - dc)
    let a1 = Pos (r1 + dr) (c1 + dc)
    let a0Valid = isValidPos grid a0
    let a1Valid = isValidPos grid a1

    case (a0Valid, a1Valid) of
        (True, True) -> [a0, a1]
        (True, False) -> [a0]
        (False, True) -> [a1]
        (False, False) -> []

-- Given a singal pair, calculate all harmonics to the edge of the grid in either direction
harmonics :: Grid -> SignalPair -> [Pos]
harmonics grid s@(SignalPair p0 p1 _) = do
    let delta@(dr,dc) = distance s
    let delta' = (-dr,-dc)
    let hpos = go [] delta p1
    let hneg = go [] delta' p0
    hpos ++ hneg
    where
        go :: [Pos] -> (Int,Int) -> Pos -> [Pos]
        go hx delta pos =
            case nextHarmonic delta pos of
                Nothing -> hx
                Just h -> go (h:hx) delta h
        nextHarmonic :: (Int,Int) -> Pos -> Maybe Pos
        nextHarmonic (dr,dc) (Pos r c) =
            let h = Pos (r+dr) (c+dc)
            in
                if isValidPos grid h then Just h else Nothing
