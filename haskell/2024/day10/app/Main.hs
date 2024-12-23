{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as V

data Loc = Loc Int Int  -- Loc row col
    deriving (Show, Eq)

data Grid = Grid {
    gridData :: V.Vector Int,
    gridWidth :: Int,
    gridHeight :: Int
} deriving (Show)

main :: IO ()
main = do
    grid <- initGrid "input"
    let trailheads = findTrailheads grid
    putStrLn $ "Trailheads: " <> show trailheads
    putStrLn ""

    let reachablePeaks = findCandidates grid <$> trailheads
    print reachablePeaks

    let part1 = sum $ length <$> nub reachablePeaks
    print part1


    let part2 = sum $ length <$> reachablePeaks
    print part2



findCandidates :: Grid -> Loc -> [Loc]
findCandidates grid loc =
    let value = elevation grid loc
    in if value == 9 then
        [loc]
       else
           let adjacents = catMaybes [north grid loc, east grid loc, south grid loc, west grid loc]
               values = fmap (elevation grid) adjacents
               nextLocs = fst <$> filter (\(_,v) -> v == (value + 1)) (zip adjacents values)
           in concatMap (findCandidates grid) nextLocs

elevation :: Grid -> Loc -> Int
elevation grid@Grid{..} loc =
    let idx = loc2Idx grid loc
    in gridData V.! idx

north :: Grid -> Loc -> Maybe Loc
north _grid (Loc row col) = if row > 0 then Just (Loc (row-1) col) else Nothing

east :: Grid -> Loc -> Maybe Loc
east Grid{..} (Loc row col) = if col < (gridWidth-1) then Just (Loc row (col+1)) else Nothing

south :: Grid -> Loc -> Maybe Loc
south Grid{..} (Loc row col) = if row < (gridHeight-1) then Just (Loc (row+1) col) else Nothing

west :: Grid -> Loc -> Maybe Loc
west _grid (Loc row col) = if col > 0 then Just (Loc row (col-1)) else Nothing

idx2Loc :: Grid -> Int -> Loc
idx2Loc Grid{..} idx =
    let row = idx `div` gridWidth
        col = idx - (row * gridWidth)
    in Loc row col

loc2Idx :: Grid -> Loc -> Int
loc2Idx Grid{..} (Loc row col) = row * gridWidth + col

findTrailheads :: Grid -> [Loc]
findTrailheads g@Grid{..} = do
    let entries = V.indexed gridData
    let trailheads = V.filter (\(_,v) -> v == 0) entries
    map (idx2Loc g . fst) (V.toList trailheads)

initGrid :: FilePath -> IO Grid
initGrid file = do
    rows <- lines <$> readFile file
    let gridWidth = length $ head rows
    let gridHeight = length rows
    let gridData = V.fromList (map (\c -> read [c]) (concat rows))
    pure Grid{..}
