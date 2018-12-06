module AdventOfCode
  ( runProgram
  , parseOnly
  , t
  , Text
  , T.unpack
  , module X
  ) where

import AdventOfCode.Parser as X
import qualified Data.Bifunctor as BF
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

t :: Show a => String -> a -> a
t s x = traceShow (s ++ ": " ++ show x) x

runProgram :: Show b => (a -> b) -> Parser a -> IO ()
runProgram f p =
  either printError (print . f) . parseInput p . T.strip . T.pack =<<
  getContents

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first errorBundlePretty . parse p ""

parseInput :: Parser a -> Text -> Either String a
parseInput p = parseOnly (p <* eof)

printError :: String -> IO ()
printError e = do
  putStrLn "There was a problem parsing the input:\n"
  putStrLn e
