{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (elemIndex, findIndex)

data Block = Block Int | Free
    deriving (Show, Eq)

main :: IO ()
main = do
    contents <- head . lines <$> readFile "input"
    --print contents
    decoded <- decodeDiskMap contents
    putStrLn ""
    --print decoded
    --putStrLn ""

    compacted <- compact decoded
    --print compacted

    let cksum = checksum compacted
    putStrLn $ show cksum


checksum :: [Block] -> Int
checksum blocks = do
    let blocksWithIndex = zip [0..] blocks
    foldl go 0 blocksWithIndex
    where
        go :: Int -> (Int, Block) -> Int
        go acc (idx, block) = do
            case block of
                Free -> acc
                Block n -> acc + (idx * n)


-- Expands the dense input format into its uncompressed form
decodeDiskMap :: String -> IO [Block]
decodeDiskMap input = go input True 0 []
    where
        go :: String -> Bool -> Int -> [Block] -> IO [Block]
        go [] _ _ decoded = pure decoded
        go (x:xs) isFile fileId decoded = do
            let n = read [x] :: Int
                (block, nextId) = if isFile then (Block fileId, fileId+1) else (Free, fileId)
                decoded' = decoded ++ replicate n block
            --putStrLn $ show x <> " : " <> xs <> "   (fileId: " <> show fileId <> ",  isFile: " <> show isFile <> ",  decoded': " <> show decoded' <> ")"
            go xs (not isFile) nextId decoded'


compact :: [Block] -> IO [Block]
compact blocks = do
    let left' = elemIndex Free blocks
    let right' = findIndex (\case
                                Free -> False
                                Block _ -> True) (reverse blocks)
    case (left', right') of
        (Nothing, _) -> pure blocks
        (_, Nothing) -> pure blocks
        (Just left, Just right'') -> do
            let right = length blocks - 1 - right''
            --putStrLn $ "left: " <> show left <> ",  right: " <> show right
            if left < right then do
                let res = swapIndex blocks left right
                --print res
                compact res
            else
                pure blocks


swapIndex :: [Block] -> Int -> Int -> [Block]
swapIndex blocks dstIdx srcIdx = do
    let src = blocks !! srcIdx
    let dst = blocks !! dstIdx
    let before = take dstIdx blocks
    let between = take (srcIdx - dstIdx - 1) (drop (dstIdx+1) blocks)
    let after = drop (srcIdx+1) blocks
    before ++ [src] ++ between ++ [dst] ++ after



