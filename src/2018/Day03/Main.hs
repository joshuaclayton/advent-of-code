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
  deriving (Eq, Show)

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

process :: [Claim] -> [Id]
process xs = (L.\\) claimIds claimIdsWithOverlap
  where
    claimIds = map (\(Claim id' _ _) -> id') xs
    claimIdsWithOverlap =
      concatMap (\(c, results) -> map fst results) $
      filter (\(c, results) -> length results > 1) $
      groupBy snd $ concatMap claimToCoordinates xs

claimToCoordinates :: Claim -> [(Id, Coordinate)]
claimToCoordinates (Claim id' (x, y) dimensions) =
  map ((id', )) $ concatMap (\x -> map (x, ) ys) xs
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
