module Main where

import Text.Regex.TDFA
import qualified Data.Text as T

main :: IO ()
main = do
    contents <- readFile "input"
    let value = parseMulAndSumProducts contents
    putStrLn $ "Sum of products: " <> show value

parseMulAndSumProducts :: String -> Int
parseMulAndSumProducts contents =
    let re = "mul\\(([0-9]+),([0-9]+)\\)"
        matches = getAllTextMatches (contents =~ re) :: [String]
        numPairs = fmap parseMul matches
        values = fmap (fmap kaboom) numPairs
        answer = sum <$> sequence values
    in
        case answer of
            Just v -> v
            Nothing -> error "No valid multyply entries"
    where
        kaboom :: (Int,Int) -> Int
        kaboom (a,b) = a * b

parseMul :: String -> Maybe (Int,Int)
parseMul x = do
    let trimmed = takeWhile (/= ')') (drop 4 x)
    let tokens = T.splitOn (T.pack ",") (T.pack trimmed)
    case tokens of
        [s0,s1] -> Just (read (T.unpack s0), read (T.unpack s1))
        _ -> Nothing
