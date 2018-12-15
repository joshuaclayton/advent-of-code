#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Control.Arrow ((&&&))
import qualified Data.Bifunctor as BF
import Data.Char
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

type TextCount = [(Char, Int)]

main :: IO ()
main = runProgram process inputParser

process :: [TextCount] -> Int
process xs = occurrences 2 xs * occurrences 3 xs
  where
    occurrences i = length . filter (hasNOccurrences i)

hasNOccurrences :: Int -> TextCount -> Bool
hasNOccurrences i = any ((== i) . snd)

inputParser :: Parser [TextCount]
inputParser = lineValue `sepBy` newline
  where
    lineValue = groupCharacterAndOccurrenceCount <$> some alphaNumChar
    groupCharacterAndOccurrenceCount = fmap (BF.second length) . groupBy id

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f =
  map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)
