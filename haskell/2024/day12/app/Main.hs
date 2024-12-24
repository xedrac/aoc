{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map as M
import Data.Map.Internal.Debug (showTree)
import Data.Maybe (catMaybes)

data Loc = Loc Int Int
    deriving (Show, Eq, Ord)

type FenceMap = M.Map Loc Int  -- Location -> Number of fence sides needed for this location
type PlotMap = M.Map Char [[Loc]]  -- Plot Type -> Group of locations for each distinct plot of this type
type ExploreMap = M.Map Loc Bool

data Grid = Grid {
    gridData :: [Char],
    gridWidth :: Int,
    gridHeight :: Int
} deriving (Show)


main :: IO ()
main = do
    grid <- readGrid "input"
    let fence = fenceMap grid
    --print fence

    let pm = plotMap grid
    --putStrLn $ showTree plots

    let plots = concat $ M.elems pm
    let part1 = sum $ fmap (price fence) plots
    print part1



price :: FenceMap -> [Loc] -> Int
price fm locs = do
    let area = length locs
    let perimiter = sum $ fmap (fm M.!) locs
    area * perimiter


plotMap :: Grid -> PlotMap
plotMap grid@Grid{..} =
    let locs = fmap (idx2Loc grid) [0..]
        zipped = zip locs gridData
        (pm, _) = foldl go (M.empty, M.empty) zipped
    in pm

    where
        go :: (PlotMap, ExploreMap) -> (Loc, Char) -> (PlotMap, ExploreMap)
        go (pm, em) (loc, c) = do
            if M.member loc em then
                (pm, em)
            else do
                let explored = explorePlot M.empty (c, loc)
                let plotLocs = M.keys explored
                let pm' = M.insertWith (++) c [plotLocs] pm
                let em' = M.union em explored
                (pm', em')

        -- Given a Loc, explore all of the tiles of the same type that are adjacent
        -- and return a list of all Locs for this plot (also mark them as explored)
        explorePlot :: ExploreMap -> (Char, Loc) -> ExploreMap
        explorePlot em (c, loc) = do
            let c' = gridData !! (loc2Idx grid loc)
            if c' == c then
                if M.member loc em then
                    em
                else do
                    let em' = M.insert loc True em
                    let locs = catMaybes [north grid loc, east grid loc, south grid loc, west grid loc]
                    let adjSame = filter (\l -> (gridData !! (loc2Idx grid l)) == c) locs
                    let adjZipped = zip (repeat c) adjSame
                    foldl explorePlot em' adjZipped
            else
                em

fenceMap :: Grid -> FenceMap
fenceMap grid@Grid{..} =
    let locs = fmap (idx2Loc grid) [0..]
        zipped = zip locs gridData
    in foldl go M.empty zipped
    where
        go :: FenceMap -> (Loc, Char) -> FenceMap
        go fm (loc, c) =
            let n = fenceNeedAtLoc grid loc c
            in M.insert loc n fm



fenceNeedAtLoc :: Grid -> Loc -> Char -> Int
fenceNeedAtLoc grid@Grid{..} loc c = do
    let adjLocs = catMaybes [north grid loc, east grid loc, south grid loc, west grid loc]
    let adjIdxs = fmap (loc2Idx grid) adjLocs
    let adjChars = fmap (gridData !!) adjIdxs
    let numSame = length $ filter (c ==) adjChars
    4 - numSame

north :: Grid -> Loc -> Maybe Loc
north _grid (Loc row col) = if row > 0 then Just (Loc (row-1) col) else Nothing

east :: Grid -> Loc -> Maybe Loc
east Grid{..} (Loc row col) = if col < (gridWidth-1) then Just (Loc row (col+1)) else Nothing

south :: Grid -> Loc -> Maybe Loc
south Grid{..} (Loc row col) = if row < (gridHeight-1) then Just (Loc (row+1) col) else Nothing

west :: Grid -> Loc -> Maybe Loc
west _grid (Loc row col) = if col > 0 then Just (Loc row (col-1)) else Nothing


readGrid :: FilePath -> IO Grid
readGrid file = do
    lines' <- lines <$> readFile file
    let gridWidth = length (head lines')
    let gridHeight = length lines'
    let gridData = concat lines'
    pure Grid{..}


idx2Loc :: Grid -> Int -> Loc
idx2Loc Grid{..} idx =
    let row = idx `div` gridWidth
        col = idx - (row * gridWidth)
    in Loc row col

loc2Idx :: Grid -> Loc -> Int
loc2Idx Grid{..} (Loc row col) = row * gridWidth + col
