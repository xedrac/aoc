module Main where

-- There's probably a *much* better way to do this, but whatever...

import Data.List (find, delete, sortBy)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

newtype Rule = Rule (Int, Int)
    deriving (Show, Eq)

newtype Blacklist = Blacklist (M.Map Int [Int])
    deriving (Show)

newtype Update = Update [Int]
    deriving (Show)


main :: IO ()
main = do
    entries <- lines <$> readFile "input"
    --mapM_ putStrLn entries

    let rx = takeWhile (not . null) entries
    let upds = drop (length rx + 1) entries
    let upds' = fmap (splitOn ",") upds
    let rules = parseRules rx
    let updates = fmap Update (fmap (fmap read) upds' :: [[Int]])
    let blklst = blacklistMap rules
    let (Just offenders) = traverse (getOffenders blklst) updates

    -- pair the updates an their offenders list together
    let updateOffenders = zip updates offenders

    let validMiddles = mapMaybe (uncurry middleNumOfValidUpdate) updateOffenders

    --putStrLn ""
    --putStrLn "Rules:"
    --print rules
    --putStrLn ""
    --putStrLn "Updates:"
    --print updates
    --putStrLn ""
    --putStrLn "Offenders:"
    --print offenders

    print $ sum validMiddles

    let invalidUpdates = fmap fst (filter (not . null . snd) updateOffenders)
    let fixedUpdates = fmap (fixUpdate rules) invalidUpdates
    let fixedMiddles = fmap middle fixedUpdates

    print $ sum fixedMiddles



-- Turns out the input is such that we can just sort based on the rules
fixUpdate :: [Rule] -> Update -> Update
fixUpdate rules (Update update) = Update $ sortBy f update
    where
        f a b = if Rule (a,b) `elem` rules then LT else GT


-- Get the middle value in a validly ordered update
middle :: Update -> Int
middle (Update update) = update !! (length update `div` 2)


-- Get the middle value in the update, but only if the update is valid
middleNumOfValidUpdate :: Update -> [(Int, Int)] -> Maybe Int
middleNumOfValidUpdate update violations = do
    if null violations then
        Just $ middle update
    else
        Nothing


getOffenders :: Blacklist -> Update -> Maybe [(Int, Int)]
getOffenders (Blacklist blklst) update = do
    let chklst = checkList update
        offending = sequenceA $ traverse Just (mapMaybe (uncurry violation) chklst)
    --traverse id $ filter isJust (fmap (uncurry violation) chklst)
    sequenceA offending

    where
        -- Pair each value with the list of values that follows it
        checkList :: Update -> [(Int, Update)]
        checkList (Update xs) = [(xs !! k, Update (drop (k+1) xs)) | k <- [0 .. (length xs - 1)]]

        -- Given a value and the list that follows it, return the first violation, if any
        violation :: Int -> Update -> Maybe (Int, Int)
        violation val (Update following) = do
            let forbidden = M.lookup val blklst
            case forbidden of
                Nothing -> Nothing
                Just blk -> do
                    let offender = find (`elem` following) blk
                    if isJust offender then do
                        off <- offender
                        Just (val, off)
                    else
                        Nothing


-- Constructs a map of a number to a list of numbers that
-- must not follow it
blacklistMap :: [Rule] -> Blacklist
blacklistMap rules = go rules (Blacklist M.empty)
    where
        go :: [Rule] -> Blacklist -> Blacklist
        go rulz (Blacklist mp) =
            if null rulz
            then
                Blacklist mp
            else
                let Rule (n,k) = head rulz
                    rest = drop 1 rulz
                in
                    go rest (Blacklist (M.insertWith (++) k [n] mp))


parseRules :: [String] -> [Rule]
parseRules xs =
    let vs = fmap (splitOn "|") xs
        hs = fmap (read . head) vs
        ts = fmap (read . last) vs
        ps = zip hs ts
    in
        fmap Rule ps
