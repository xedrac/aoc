module Main where

import Text.Megaparsec
import Text.Megaparsec.Char as M
import Data.Void
import Control.Monad (void)


type Parser = Parsec Void String

data Value
    = ValueMul (Int,Int)
    | ValueDont
    | ValueDo
    | ValueGarbage
    deriving (Show, Eq)


main :: IO ()
main = do
    contents <- readFile "input"
    --let value = parseMulAndSumProducts contents
    --putStrLn $ "Sum of products: " <> show value

    let values = case runParser parseAll "" contents of
                    Right xs -> xs
                    _ -> error "failed to parse"

    let part1 = foldl multiplyAll [] values
    print $ sum part1

    let relevant = foldl multiplyBetweenDo (True, []) values
    print $ sum (snd relevant)

    where
        multiplyAll :: [Int] -> Value -> [Int]
        multiplyAll acc val = do
            case val of
                ValueMul (a, b) -> (a * b) : acc
                _ -> acc
        multiplyBetweenDo :: (Bool, [Int]) -> Value -> (Bool, [Int])
        multiplyBetweenDo acc val = do
            let active = fst acc
                xs = snd acc
            case val of
                ValueMul (a, b) -> if active then (active, (a * b) : xs) else (active, xs)
                ValueDo -> (True, xs)
                ValueDont -> (False, xs)
                ValueGarbage -> (active, xs)


valueParser :: Parser Value
valueParser =
    mulParser <|>
    dontParser <|>
    doParser <|>
    garbageParser

mulParser :: Parser Value
mulParser = try $ do
    void $ M.string "mul("
    a <- many M.digitChar
    void $ M.string ","
    b <- many M.digitChar
    void $ M.string ")"
    pure $ ValueMul (read a, read b)

dontParser :: Parser Value
dontParser = try $ do
    void $ M.string "don't()"
    pure ValueDont

doParser :: Parser Value
doParser = try $ do
    void $ M.string "do()"
    pure ValueDo

garbageParser :: Parser Value
garbageParser = do
    void M.asciiChar
    pure ValueGarbage

parseAll :: Parser [Value]
parseAll = many valueParser





-- Original part1 using regex

--import Text.Regex.TDFA
--import qualified Data.Text as T

--parseMulAndSumProducts :: String -> Int
--parseMulAndSumProducts contents =
--    let re = "mul\\(([0-9]+),([0-9]+)\\)"
--        matches = getAllTextMatches (contents =~ re) :: [String]
--        numPairs = fmap parseMul matches
--        values = fmap (fmap kaboom) numPairs
--        answer = sum <$> sequence values
--    in
--        case answer of
--            Just v -> v
--            Nothing -> error "No valid multyply entries"
--    where
--        kaboom :: (Int,Int) -> Int
--        kaboom (a,b) = a * b
--
--parseMul :: String -> Maybe (Int,Int)
--parseMul x = do
--    let trimmed = takeWhile (/= ')') (drop 4 x)
--    let xs = T.splitOn (T.pack ",") (T.pack trimmed)
--    case xs of
--        [s0,s1] -> Just (read (T.unpack s0), read (T.unpack s1))
--        _ -> Nothing
