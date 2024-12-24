module Main where

import qualified Data.Map as M

type StoneMap = M.Map Int Int

main :: IO ()
main = do
    stones <- parseInput
    let part1 = sum $ blinkNTimes 25 stones
    print part1

    let part2 = sum $ blinkNTimes 75 stones
    print part2

parseInput :: IO StoneMap
parseInput = do
    numbers <- fmap read . words <$> readFile "input"
    pure $ M.fromList [(key, 1) | key <- numbers]

blinkNTimes :: Int -> StoneMap -> StoneMap
blinkNTimes 0 stones = stones
blinkNTimes iter stones = blinkNTimes (iter-1) (blinkMap stones)

blinkMap :: StoneMap -> StoneMap
blinkMap before = M.fromListWith (+) [(newStone, count) | (stone, count) <- M.assocs before, newStone <- blinkStone stone]

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone stone = do
    let s = show stone
        n = length s
    if even n then
        let half = n `div` 2
            left = read $ take half s :: Int
            right = read $ drop half s :: Int
        in [left, right]
    else
        [stone * 2024]
