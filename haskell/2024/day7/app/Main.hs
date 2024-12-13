{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)

data Oper = Add | Mult | Join
    deriving (Show)

main :: IO ()
main = do
    lines' <- lines <$> readFile "input"
    let tokens = fmap (fmap words . splitOn ":") lines'
    let numTokens = fmap (fmap (fmap read)) tokens :: [[[Int]]]
    let values = concatMap head numTokens
    let nums = fmap last numTokens
    let equations = zip values nums

    part1 equations
    part2 equations

part1 :: [(Int, [Int])] -> IO ()
part1 equations = do
    let ops = [Mult, Add]
    let res = fmap (uncurry (findOpers ops)) equations
    let valid = sequenceA $ filter isJust res
    mapM_ (putStrLn . show) valid
    case valid of
        Nothing -> putStrLn "Part1: No Solution"
        Just vs -> putStrLn $ "Part1: " <> show (sum (fmap fst vs))

part2 :: [(Int, [Int])] -> IO ()
part2 equations = do
    let ops = [Mult, Add, Join]
    res <- mapConcurrently (pure . uncurry (findOpers ops)) equations
    let valid = sequenceA $ filter isJust res
    mapM_ (putStrLn . show) valid
    case valid of
        Nothing -> putStrLn "Part2: No Solution"
        Just vs -> putStrLn $ "Part2: " <> show (sum (fmap fst vs))

-- Given a list of numbers, find the sequence of operators that produces the given test value
findOpers :: [Oper] -> Int -> [Int] -> Maybe (Int, [Oper])
findOpers opPool value nums = do
    let opPerms = operPermutations (length nums - 1) opPool
    let res = fmap ((\(ops,x) -> if x == value then Just ops else Nothing) . compute nums) opPerms
    let solutions = filter isJust res
    if null solutions then
        Nothing
    else do
        case head solutions of
            Nothing -> Nothing
            Just xs -> Just (value, xs)

compute :: [Int] -> [Oper] -> ([Oper], Int)
compute nums opers =
    let acc = head nums :: Int
        terms = zip opers (tail nums) :: [(Oper, Int)]
        value = foldl calc acc terms
    in
        (opers, value)

calc :: Int -> (Oper, Int) -> Int
calc acc (op, val) =
    case op of
        Add -> acc + val
        Mult -> acc * val
        Join -> read (show acc ++ show val)

-- Generate op lists of length 'size' such that every
-- permutation of operators is represented in the output
operPermutations :: Int -> [Oper] -> [[Oper]]
operPermutations = replicateM
