module Main where

import Data.List (sort, transpose)
import Data.List.Split

main :: IO ()
main = do
    (left, right) <- parseLists "input"
    let distances = fmap abs (zipWith subtract right left)
    putStrLn $ "Value: " ++ show (sum distances)

    let score = similarityScore left right
    putStrLn $ "Similarity: " ++ show score

parseLists :: FilePath -> IO ([Int], [Int])
parseLists inputFile = do
    contents <- readFile inputFile
    let entries = lines contents
    mapM_ (putStrLn . show) entries
    let (left:right:_) = transpose $ fmap (splitOn "   ") entries
    let left' = sort $ fmap read left
    let right' = sort $ fmap read right
    mapM_ (\(x,y) -> putStrLn $ show x ++ " " ++ show y ++ " = " ++ show (abs (subtract x y))) (zip left' right')
    pure (left', right')


similarityScore :: [Int] -> [Int] -> Int
similarityScore left right = do
    let counts = fmap multiplyIntstances left
    sum $ zipWith (*) left counts
    where
        multiplyIntstances :: Int -> Int
        multiplyIntstances value = length $ filter (== value) right
