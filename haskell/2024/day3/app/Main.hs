module Main where

import Text.Regex.TDFA
import qualified Data.Text as T

main :: IO ()
main = do
    contents <- readFile "input"
    let re = "mul\\(([0-9]+),([0-9]+)\\)"
    let matches = getAllTextMatches (contents =~ re) :: [String]
    let numPairs = fmap parseMul matches
    let values = fmap kaboom numPairs
    --putStrLn $ show values
    putStrLn $ show (fmap sum $ sequence values)
    where
        -- I shouldn't have to operate on Maybe's here...
        kaboom :: Maybe (Int, Int) -> Maybe Int
        kaboom (Just (a,b)) = Just (a * b)
        kaboom Nothing = Nothing

parseMul :: String -> Maybe (Int,Int)
parseMul x = do
    let trimmed = takeWhile (/= ')') (drop 4 x)
    let tokens = T.splitOn (T.pack ",") (T.pack trimmed)
    case tokens of
        [s0,s1] -> Just (read (T.unpack s0), read (T.unpack s1))
        _ -> Nothing
