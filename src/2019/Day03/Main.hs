#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc 
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import qualified Data.Bifunctor as BF
import Data.Char
import Data.Functor (($>))
import qualified Data.Maybe as M
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction
    = U
    | R
    | D
    | L
    deriving (Show)

data Instruction =
    Instruction Direction
                Int
    deriving (Show)

type Coordinate = (Int, Int)

main :: IO ()
main = runProgram process inputParser

tupleToList :: (a, a) -> [a]
tupleToList (x, y) = x : [y]

process :: ([Instruction], [Instruction]) -> Int
process =
    minimum .
    filter (/= 0) .
    map coordinateToDistance .
    coordinateOverlaps .
    tupleToList . BF.bimap instructionsToCoordinates instructionsToCoordinates

coordinateToDistance :: Coordinate -> Int
coordinateToDistance (x, y) = abs x + abs y

coordinateOverlaps :: [[Coordinate]] -> [Coordinate]
coordinateOverlaps = Set.toList . foldl1 Set.intersection . map Set.fromList

instructionsToCoordinates :: [Instruction] -> [Coordinate]
instructionsToCoordinates = concat . scanl instructionToCoordinate [(0, 0)]
  where
    instructionToCoordinate xs instruction =
        case instruction of
            (Instruction R i) -> [(x + n, y) | n <- [1 .. i]]
            (Instruction U i) -> [(x, y + n) | n <- [1 .. i]]
            (Instruction D i) -> [(x, y - n) | n <- [1 .. i]]
            (Instruction L i) -> [(x - n, y) | n <- [1 .. i]]
      where
        (x, y) = last xs

inputParser :: Parser ([Instruction], [Instruction])
inputParser = do
    firstWire <- instructionsParser
    _ <- newline
    secondWire <- instructionsParser
    return (firstWire, secondWire)

instructionsParser :: Parser [Instruction]
instructionsParser = instructionParser `sepBy` char ','

instructionParser :: Parser Instruction
instructionParser = Instruction <$> directionParser <*> intParser

directionParser :: Parser Direction
directionParser =
    (string "R" $> R) <|> (string "U" $> U) <|> (string "D" $> D) <|>
    (string "L" $> L)
