module Main where

--import Text.Regex.Compat
import Text.Regex.Posix ((=~), AllMatches (..))

findMatches :: String -> String -> [String]
findMatches input re = getAllMatches' (input =~ re :: AllMatches [] String)

getAllMatches' :: AllMatches [] String -> [String]
getAllMatches' (AllMatches matches) = matches

main :: IO ()
main = do
    contents <- readFile "input"
    let re = "mul([0-9]+,[0-9]+)"
    let matches = findMatches contents re
    mapM_ putStrLn matches
