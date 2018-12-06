#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc 
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Data.Char
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = runProgram process inputParser

process :: [Integer] -> Maybe Integer
process = firstDuplicate . scanl (+) 0 . cycle

inputParser :: Parser [Integer]
inputParser = lineValue `sepBy` newline
  where
    lineValue = try positiveIntegerParser <|> signedIntegerParser

positiveIntegerParser :: Parser Integer
positiveIntegerParser = char '+' *> integerParser <?> "positive number"

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go acc [] = Nothing
    go acc (x:xs) =
      if x `Set.member` acc
        then Just x
        else go (Set.insert x acc) xs
