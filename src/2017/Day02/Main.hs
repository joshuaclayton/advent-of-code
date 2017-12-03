#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

main :: IO ()
main = runProgram process inputParser

process :: [[Int]] -> Int
process = sum . map checksum

checksum :: [Int] -> Int
checksum [] = 0
checksum xs = head $ Data.Maybe.catMaybes evenSplits
  where
    evenSplits = map evenSplit pairs
    pairs = [(a, b) | a <- xs, b <- reverse xs]

evenSplit :: (Int, Int) -> Maybe Int
evenSplit (a, b) =
    if a == b
        then Nothing
        else case a `divMod` b of
                 (res, 0) -> Just res
                 _ -> Nothing

inputParser :: Parser [[Int]]
inputParser = lineParser `sepBy` newline

lineParser :: Parser [Int]
lineParser = map fromIntegral <$> L.decimal `sepBy` tab
