#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import AdventOfCode
import Control.Arrow ((&&&))
import qualified Data.Bifunctor as BF
import Data.Function (on)
import qualified Data.List as L
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Id =
  Id Int
  deriving (Show)

data Dimensions = Dimensions
  { width :: Int
  , height :: Int
  } deriving (Show)

type Coordinate = (Int, Int)

data Claim =
  Claim Id
        Coordinate
        Dimensions
  deriving (Show)

main :: IO ()
main = runProgram process inputParser

process :: [Claim] -> Int
process xs =
  length $
  filter (\(_, i) -> i > 1) $
  map (BF.second length) $ groupBy id $ concatMap claimToCoordinates xs

claimToCoordinates :: Claim -> [Coordinate]
claimToCoordinates (Claim _ (x, y) dimensions) =
  concatMap (\x -> map (x, ) ys) xs
  where
    xs = [x .. (x + width dimensions - 1)]
    ys = [y .. (y + height dimensions - 1)]

inputParser :: Parser [Claim]
inputParser = claimParser `sepBy` newline

claimParser :: Parser Claim
claimParser =
  Claim <$> (idParser <* space <* char '@' <* space) <*>
  (coordinateParser <* char ':' <* space) <*>
  dimensionsParser

idParser :: Parser Id
idParser = Id <$> (char '#' *> intParser)

coordinateParser :: Parser Coordinate
coordinateParser = (,) <$> (intParser <* char ',') <*> intParser

dimensionsParser :: Parser Dimensions
dimensionsParser = Dimensions <$> (intParser <* char 'x') <*> intParser

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f =
  map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)
