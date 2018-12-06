#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = runProgram process inputParser

process :: [Int] -> Int
process xs = go 0 [] xs
  where
    go pos result (a:rest) =
      if a == cycle xs !! (pos + length xs `quot` 2)
        then go (pos + 1) (a : result) rest
        else go (pos + 1) result rest
    go _ result [] = sum result

inputParser :: Parser [Int]
inputParser = map digitToInt <$> many digitChar
