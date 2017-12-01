module AdventOfCode
    ( runProgram
    , parseOnly
    , Text
    , T.unpack
    ) where

import qualified Data.Bifunctor as BF
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text

runProgram :: Show b => (a -> b) -> Parser a -> IO ()
runProgram f p =
    either (printError . show) (print . f) . parseInput p . T.strip . T.pack =<< getContents

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first parseErrorPretty . runParser p ""

parseInput :: Parser a -> Text -> Either String a
parseInput p = parseOnly (p <* eof)

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e
