module Main where

import Prelude hiding (lookup)
import Data.HashMap.Strict (fromList, lookup)
import System.Environment

type WordString = String
type Score = Int

letterScores = fromList $ zip ['a'..'z'] [1..]

wordScore :: WordString -> Maybe Score
wordScore w = sum <$> mapM (\x -> lookup x letterScores) w

score100 :: [(WordString, Maybe Score)] -> [WordString]
score100 l = map fst $ filter (\(_, x) -> x == Just 100) l

main :: IO ()
main = do
    (filePath:_) <- getArgs
    f <- readFile filePath
    let wordList = words f
    let scoreList = wordScore <$> wordList
    let wordScoreList = zip wordList scoreList
    let words100Score = score100 wordScoreList
    putStrLn $ "Number of words with a score of 100: " ++ show (length words100Score)
    putStrLn "Words with a score of 100:"
    print words100Score
