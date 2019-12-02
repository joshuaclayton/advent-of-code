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

process :: [Integer] -> Integer
process = sum . map fuelRequired

fuelRequired :: Integer -> Integer
fuelRequired i = floor (fromIntegral i / 3) - 2

inputParser :: Parser [Integer]
inputParser = lineValue `sepBy` newline
  where
    lineValue = try positiveIntegerParser <|> signedIntegerParser

positiveIntegerParser :: Parser Integer
positiveIntegerParser = char '+' *> integerParser <?> "positive number"
