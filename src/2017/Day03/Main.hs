#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Data.Char
import Data.Maybe
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

main :: IO ()
main = runProgram process inputParser

process :: Int -> Int
process = manhattanDistance . traverseSpiral

inputParser :: Parser Int
inputParser = fromIntegral <$> L.decimal

type Point = (Int, Int)

data Direction
    = R
    | U
    | L
    | D
    | Origin

traverseSpiral :: Int -> Point
traverseSpiral endingItem = go 0 (0, 0) movementsWithDirections
  where
    go :: Int -> Point -> [(Int, Direction)] -> Point
    go starting currentPosition ((0, currentDirection):remainingMovementsWithDirections) =
        if starting == (endingItem - 1)
            then currentPosition
            else go starting currentPosition remainingMovementsWithDirections
    go starting currentPosition ((x, currentDirection):remainingMovementsWithDirections) =
        if starting == (endingItem - 1)
            then currentPosition
            else go
                     (starting + 1)
                     (move currentDirection currentPosition)
                     ((x - 1, currentDirection) : remainingMovementsWithDirections)

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

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
