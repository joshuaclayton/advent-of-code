#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Data.Maybe
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = runProgram process inputParser

process :: Int -> Int
process = fst . traverseSpiral

inputParser :: Parser Int
inputParser = fromIntegral <$> L.decimal

type Point = (Int, Int)

data Direction
  = R
  | U
  | L
  | D
  | Origin

type ScoredPoint = (Int, Point)

calculateScore :: Point -> [ScoredPoint] -> Int
calculateScore (0, 0) = const 1
calculateScore point = totalPoints . filter applicableScoredPoints
  where
    applicableScoredPoints (_, point') = adjacentPoint point point'
    totalPoints = sum . fmap fst

adjacentPoint :: Point -> Point -> Bool
adjacentPoint (x, y) (x', y') = abs (x - x') < 2 && abs (y - y') < 2

traverseSpiral :: Int -> ScoredPoint
traverseSpiral endingItem = go [] 0 (0, 0) movementsWithDirections
  where
    go :: [ScoredPoint] -> Int -> Point -> [(Int, Direction)] -> ScoredPoint
    go scoredPoints starting currentPosition ((0, currentDirection):remainingMovementsWithDirections) =
      if not (null scoredPoints) && endingItem < fst (head scoredPoints)
        then head scoredPoints
        else go
               scoredPoints
               starting
               currentPosition
               remainingMovementsWithDirections
    go scoredPoints starting currentPosition ((x, currentDirection):remainingMovementsWithDirections) =
      if not (null scoredPoints) && endingItem < fst (head scoredPoints)
        then head scoredPoints
        else go
               ((calculateScore currentPosition scoredPoints, currentPosition) :
                scoredPoints)
               (starting + 1)
               (move currentDirection currentPosition)
               ((x - 1, currentDirection) : remainingMovementsWithDirections)

move :: Direction -> Point -> Point
move R (x, y) = (x + 1, y)
move U (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move D (x, y) = (x, y - 1)
move Origin (x, y) = (0, 0)

movementsWithDirections :: [(Int, Direction)]
movementsWithDirections = (0, Origin) : movements `zip` cycle [R, U, L, D]

movements :: [Int]
movements = mconcat [[x, x] | x <- [1 ..]]
