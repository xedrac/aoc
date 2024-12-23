{-# LANGUAGE LambdaCase #-}

-- Uggh, this is HORRIBLY ineffecient.  I should rework this to use mutable vectors or something...

module Main where

import Data.Char
import Data.List (elemIndex, findIndex)
--import Text.Printf (printf)

data Block = Block Int Bool | Free
    deriving (Show, Eq)

data Span = Span {
    blk :: Block,
    len :: Int
} deriving (Show)


main :: IO ()
main = do
    contents <- head . lines <$> readFile "input"
    --print contents
    blocks <- decodeDiskMap contents
    putStrLn ""
    --print decoded
    --putStrLn ""

    --compacted <- compactBlocks blocks
    --print compacted

    --let cksum = checksum compacted
    --print cksum

    let spans = blocksToSpans blocks
    --printSpans spans
    compactedSpans <- compactSpans spans 0 (length spans - 1)
    --printSpans compactedSpans

    let cksum2 = checksum $ spansToBlocks compactedSpans
    print cksum2


checksum :: [Block] -> Int
checksum blocks = do
    let blocksWithIndex = zip [0..] blocks
    foldl go 0 blocksWithIndex
    where
        go :: Int -> (Int, Block) -> Int
        go acc (idx, block) = do
            case block of
                Free -> acc
                Block n _ -> acc + (idx * n)


-- Expands the dense input format into its uncompressed form
decodeDiskMap :: String -> IO [Block]
decodeDiskMap input = go input True 0 []
    where
        go :: String -> Bool -> Int -> [Block] -> IO [Block]
        go [] _ _ decoded = pure decoded
        go (x:xs) isFile fileId decoded = do
            let n = read [x] :: Int
                (block, nextId) = if isFile then (Block fileId False, fileId+1) else (Free, fileId)
                decoded' = decoded ++ replicate n block
            --putStrLn $ show x <> " : " <> xs <> "   (fileId: " <> show fileId <> ",  isFile: " <> show isFile <> ",  decoded': " <> show decoded' <> ")"
            go xs (not isFile) nextId decoded'


compactBlocks :: [Block] -> IO [Block]
compactBlocks blocks = do
    let left' = elemIndex Free blocks
    let right' = findIndex (\case
                                Free -> False
                                Block _ _ -> True) (reverse blocks)
    case (left', right') of
        (Nothing, _) -> pure blocks
        (_, Nothing) -> pure blocks
        (Just left, Just right'') -> do
            let right = length blocks - 1 - right''
            --putStrLn $ "left: " <> show left <> ",  right: " <> show right
            if left < right then do
                let res = swapIndex blocks left right
                --print res
                compactBlocks res
            else
                pure blocks


blocksToSpans :: [Block] -> [Span]
blocksToSpans blocks = reverse $ foldl go [] blocks
    where
        go :: [Span] -> Block -> [Span]
        go [] b = [Span b 1]
        go (s@(Span b' l):spx) b = do
            if b == b' then
                Span b (l+1) : spx  -- increase length of current span
            else
                Span b 1 : s : spx  -- add a new span

spansToBlocks :: [Span] -> [Block]
spansToBlocks = foldl go []
    where
        go :: [Block] -> Span -> [Block]
        go acc s =
            case s of
                (Span Free l) -> acc ++ replicate l Free
                (Span b l) -> acc ++ replicate l b

printMoves :: [Block] -> IO ()
printMoves [] = putStrLn ""
printMoves (b:bs) = do
    case b of
        Block _ True -> putStr "*"
        Block _ False -> putStr " "
        Free -> putStr " "
    printMoves bs


printSpanMoves :: [Span] -> IO ()
printSpanMoves = printMoves . spansToBlocks


printBlocks :: [Block] -> IO ()
printBlocks [] = putStrLn ""
printBlocks (b:bs) = do
    case b of
        Block k _ -> putStr [idToChar k]
        Free -> putStr "."
    printBlocks bs

idToChar :: Int -> Char
idToChar k = chr $ if k < 10 then k + 48 else k + 55

printSpans :: [Span] -> IO ()
printSpans = printBlocks . spansToBlocks


compactSpans :: [Span] -> Int -> Int -> IO [Span]
compactSpans spans j k =
    if j < k then do
        let nextFreeIdx = nextFreeBefore spans j k
        case nextFreeIdx of
            Nothing -> compactSpans spans k k
            Just idx0 -> do
                let s0@(Span _ len0) = spans !! idx0
                --printf "%02d: %s\n" idx0 (replicate len0 '.')
                let nextBlockIdx = nextBlockAfter spans k j
                case nextBlockIdx of
                    Nothing -> pure spans
                    Just idx1 -> do
                        let s1@(Span (Block _id' _) len1) = spans !! idx1
                        --printf "%02d: %s\n" idx1 (concat (replicate len1 [idToChar _id']))
                        if len0 >= len1 then do
                            let spans' = splitSpan (idx0, s0) (idx1, s1)
                            --printf "fits!\n"
                            --printSpans spans'
                            --printSpanMoves spans'
                            compactSpans spans' 0 idx1
                        else do
                            --printf "doesn't fit\n"
                            compactSpans spans (idx0+1) idx1
    else do
        let firstFreeIdx = nextFreeBefore spans 0 k
        case firstFreeIdx of
            Nothing -> do
                --printf "No free space remaining\n"
                pure spans
            Just j' -> do
                --printf "Exhausted possibilities for %d\n" k
                case nextBlockAfter spans (k-1) j' of
                    Nothing -> pure spans
                    Just k' -> compactSpans spans j' k'

  where
    nextFreeBefore :: [Span] -> Int -> Int -> Maybe Int
    nextFreeBefore spx idx idxMax = do
        if idx >= 0 && idx < idxMax then
            case spx !! idx of
                (Span Free _) -> Just idx
                _ -> nextFreeBefore spx (idx+1) idxMax
        else
            Nothing

    nextBlockAfter :: [Span] -> Int -> Int -> Maybe Int
    nextBlockAfter spx idx idxMin = do
        if idx < length spx && idx > idxMin then
            case spans !! idx of
                (Span (Block _ m) _) -> if m then nextBlockAfter spx (idx-1) idxMin else Just idx
                (Span Free _) -> nextBlockAfter spx (idx-1) idxMin
        else
            Nothing

    splitSpan :: (Int, Span) -> (Int, Span) -> [Span]
    splitSpan (_, Span (Block _ _) _) _ = spans
    splitSpan _ (_, Span Free _) = spans
    splitSpan (idx0, Span Free len0) (idx1, s1@(Span (Block _ _) len1)) = do
        let remain = len0 - len1
            before = take idx0 spans
            marked = markMove s1
            mid = if remain > 0 then marked : [Span Free remain] else [marked]
            after = drop (idx0+1) (take idx1 spans)
            free = Span Free len1 : drop (idx1+1) spans
        before ++ mid ++ after ++ free

markMove :: Span -> Span
markMove (Span (Block xid _) len') = Span (Block xid True) len'
markMove sp = sp

swapIndex :: [a] -> Int -> Int -> [a]
swapIndex xs dstIdx srcIdx = do
    let src = xs !! srcIdx
    let dst = xs !! dstIdx
    let before = take dstIdx xs
    let between = take (srcIdx - dstIdx - 1) (drop (dstIdx+1) xs)
    let after = drop (srcIdx+1) xs
    before ++ [src] ++ between ++ [dst] ++ after
