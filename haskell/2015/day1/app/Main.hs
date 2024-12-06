module Main where

main :: IO ()
main = do
    input <- readInput "input"
    putStrLn $ "Floor: " ++ show (findFloor input)
    putStrLn $ "Entered Basement at index: " ++ show (enteredBasementAt input)

-- Reads all parens into one long string, where ( indicates going up 1 level, and ) going down 1 level
readInput :: String -> IO String
readInput path = do
    contents <- readFile path
    pure $ filter (\c -> c == '(' || c == ')') contents

-- Traverses all of the floor changes and reports what level we end up on
findFloor :: String -> Int
findFloor input =
    let rise = length $ filter (== '(') input
        fall = length $ filter (== ')') input
    in
        rise - fall

-- Reports the index at which the current level drops below grade
enteredBasementAt :: String -> Maybe Int
enteredBasementAt input = go input 0 0
    where
        go :: String -> Int -> Int -> Maybe Int
        go _ (-1) index = Just index
        go [] _ _ = Nothing
        go [x] _ index = if x == ')' then Just index else Nothing
        go (x:xs) level index = do
            let next = if x == '(' then (+1) else subtract 1
            go xs (next level) (index+1)
