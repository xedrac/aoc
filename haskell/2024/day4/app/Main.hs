module Main where

import Data.List

main :: IO ()
main = do
    --let rows = ["0123",
    --            "4567",
    --            "89AB",
    --            "CDEF"]
    --rows <- lines <$> readFile "input.small"
    rows <- lines <$> readFile "input"
    let cols = transpose rows
    let diags = diagonals rows

    let zx = rows ++ cols ++ diags
    let zxx = zx ++ fmap reverse zx

    putStrLn "Rows:"
    mapM_ print rows
    putStrLn ""
    putStrLn "Cols:"
    mapM_ print cols
    putStrLn ""
    putStrLn "Diags:"
    mapM_ print diags
    putStrLn ""
    putStrLn "Rows':"
    mapM_ print (fmap reverse rows)
    putStrLn ""
    putStrLn "Cols':"
    mapM_ print (fmap reverse cols)
    putStrLn ""
    putStrLn "Diags':"
    mapM_ print (fmap reverse diags)

    let numhits = sum (fmap (countSubstring "XMAS") zxx)
    putStrLn $ "XMAS count: " ++ show numhits


-- Returns True if s0 is found anywhere in s1
countSubstring :: String -> String -> Int
xs `countSubstring` ys = length $ filter id (fmap (xs `isPrefixOf`) (tails ys))


diagonals :: [[Char]] -> [[Char]]
diagonals rows = do
    let topr = topRightTriangle rows
    let botl = topRightTriangle (transpose rows)
    let topl = reverse $ topLeftTriangle rows
    let botr = transpose $ topLeftTriangle (transpose (reverse (fmap reverse rows)))

    let rd1 = transpose topr
    let rd2 = tail $ transpose botl
    let ld1 = transpose (transpose (reverse topl))
    let ld2 = tail $ transpose botr
    -- Collect all of the diagnals togther, then append their reverses as well
    -- so we can easily search in reverse
    rd1 ++ rd2 ++ ld1 ++ ld2


-- Returns a list of rows that represent the top right of the matrix.
-- This is useful to construct a list of diagonals moving right and down.
-- We can get the other half of the triangle by transposing the matrix
--   A B C D        A B C D
--   E F G H   =>     F G H
--   I J K L            K L
--   M N O P              P
topRightTriangle :: [[Char]] -> [[Char]]
topRightTriangle = go []
    where
        go :: [[Char]] -> [[Char]] -> [[Char]]
        go acc xs =
            if null xs then
                acc
            else
                let row = take 1 xs
                    rest = filter (not . null) (fmap tail (drop 1 xs))
                in
                    go (acc ++ row) rest


-- Returns a list of row that represent the top left of the matrix.
-- This is useful to contruct a list of diagonals moving left and down.
-- We can get the other half of the triangle by reversing rows and cols
-- of the original transpotition
topLeftTriangle :: [[Char]] -> [[Char]]
topLeftTriangle = go []
    where
        go :: [[Char]] -> [[Char]] -> [[Char]]
        go acc xs =
            if null xs then
                acc
            else
                let row = take 1 xs
                    rest = filter (not . null) (fmap init (drop 1 xs))
                in
                    go (row ++ acc) rest


--diagonalRight :: [[a]] -> [[a]]
--diagonalRight xxs =
--    [ [ xx !! (i + j) | i <- [0..min (length xx - 1) (length (head xxs) - 1 - j)] ]
--    | j <- [0..(length (head xxs) - 1)], xx <- xxs ]
--
--
--diagonalLeft :: [[a]] -> [[a]]
--diagonalLeft xxs =
--    [ [ xx !! (i + j) | i <- [0..min (length xx - 1) (length (head xxs) - 1 - j)] ]
--    | j <- [0..(length (head xxs) - 1)], xx <- reverse xxs ]


