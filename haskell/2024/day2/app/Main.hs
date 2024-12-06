module Main where

import Data.List
import Data.List.Split

main :: IO ()
main = do
    reports <- parseReports "input"
    --mapM_ (putStrLn . show) (take 10 reports)
    let xs = fmap (\r -> (isSafe r, r)) reports
    --mapM_ (putStrLn . show) xs
    let safes = filter id (fmap fst xs)
    putStrLn $ "Safe count: " ++ show (length safes)


    let allCandidates = fmap candidateReports reports
    let safeCandidates = fmap hasSafeCandidate allCandidates
    let n = length $ filter id safeCandidates
    putStrLn $ "Tolerantly Safe count: " ++ show n


parseReports :: FilePath -> IO [[Int]]
parseReports inputFile = do
    contents <- readFile inputFile
    let entries = lines contents
    let xs = fmap (splitOn " ") entries
    let reports = fmap (fmap read) xs
    --mapM_ (putStrLn . show) reports
    pure reports

isSafe :: [Int] -> Bool
isSafe report =
    let sorted = (sort . nub) report
        reversed = reverse sorted
    in
        (report == sorted || report == reversed) && slowEnough sorted

slowEnough :: [Int] -> Bool
slowEnough report =
    let xs = abs <$> zipWith subtract report (drop 1 report)
    in
        not $ any (>3) xs

-- Given a report, return a list of all possible candidate tolerant reports, including the unmodified original
candidateReports :: [Int] -> [[Int]]
candidateReports report = reverse $ go report 0 [report]
    where
        go :: [Int] -> Int -> [[Int]] -> [[Int]]
        go r idx tolerants =
            let candidate = dropAt idx r
            in
                if idx >= length r then
                    tolerants
                else
                    go r (idx+1) (candidate : tolerants)
        dropAt :: Int -> [Int] -> [Int]
        dropAt i r = take i r ++ drop (i+1) r

hasSafeCandidate :: [[Int]] -> Bool
hasSafeCandidate = any isSafe
