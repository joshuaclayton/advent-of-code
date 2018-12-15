#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Control.Arrow ((&&&))
import qualified Data.Bifunctor as BF
import Data.Char
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = runProgram process inputParser

process :: [String] -> [(String, String, Bool)]
process xs =
  filter (\(_, _, v) -> v == True) $
  map (\(p1, p2) -> (p1, p2, singleCharacterDifference p1 p2)) $ pairs xs

pairs :: [a] -> [(a, a)]
pairs xs = foldl (\acc i -> (map (\v -> (i, v)) xs) ++ acc) [] xs

singleCharacterDifference :: String -> String -> Bool
singleCharacterDifference s1 s2 = onlyOneMatch $ matches <$> pairs
  where
    pairs = zip s1 s2
    matches = uncurry (==)
    onlyOneMatch xs = (length $ filter (== False) xs) == 1

inputParser :: Parser [String]
inputParser = lineValue `sepBy` newline
  where
    lineValue = some alphaNumChar
