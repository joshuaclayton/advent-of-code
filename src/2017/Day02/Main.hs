#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

main :: IO ()
main = runProgram process inputParser

process :: [[Int]] -> Int
process = sum . map checksum

checksum :: [Int] -> Int
checksum [] = 0
checksum xs = maximum xs - minimum xs

inputParser :: Parser [[Int]]
inputParser = lineParser `sepBy` newline

lineParser :: Parser [Int]
lineParser = map fromIntegral <$> L.decimal `sepBy` tab
