#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Data.Time as Time
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = runProgram process inputParser

newtype Id =
  Id Int

data Period
  = Awake UTCTime
          UTCTime
  | Asleep UTCTime
           UTCTime

data Assignment =
  Assignment Id
             [Period]

process :: [Assignment] -> Int
process xs = 5

inputParser :: Parser [Assignment]
inputParser = some assignmentParser

assignmentParser :: Parser Assignment
assignmentParser = undefined

timestampParser :: Parser UTCTime
timestampParser = Time.UTCTime <$> (dateParser <* char ' ') <*> timeParser

dateParser :: Parser Time.Day
dateParser =
  Time.fromGregorian <$> (integerParser <* char '-') <*> (intParser <* char '-') <*>
  intParser

timeParser :: Parser Time.DiffTime
timeParser = do
  hoursInSeconds <- (60 *) <$> integerParser
  _ <- char ':'
  seconds <- integerParser
  pure $ Time.secondsToDiffTime $ hoursInSeconds + seconds
