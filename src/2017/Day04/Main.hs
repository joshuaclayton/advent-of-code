#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import qualified Data.List as L
import Text.Megaparsec
import Text.Megaparsec.Text

main :: IO ()
main = runProgram process inputParser

process :: [[String]] -> Int
process = length . filter validPassphrase

validPassphrase :: [String] -> Bool
validPassphrase xs = length xs == length (L.nub xs)

inputParser :: Parser [[String]]
inputParser = lineParser `sepBy` newline

lineParser :: Parser [String]
lineParser = many alphaNumChar `sepBy` char ' '