#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Text

main :: IO ()
main = runProgram printSuccess parseInput

printSuccess :: [Int] -> IO ()
printSuccess = print . process

process :: [Int] -> Int
process xs = go [] xs
  where
    go result (a:b:rest) =
        if a == b
            then go (a : result) (b : rest)
            else go result (b : rest)
    go result [a] =
        if a == last xs
            then go (a : result) []
            else sum result
    go result [] = sum result

parseInput :: Text -> Either String [Int]
parseInput = parseOnly (inputParser <* eof)

inputParser :: Parser [Int]
inputParser = map digitToInt <$> many digitChar
