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

runProgram :: Show b => (a -> IO ()) -> (Text -> Either b a) -> IO ()
runProgram f parseInput =
    either (printError . show) f . parseInput . T.strip . T.pack =<< getContents

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first parseErrorPretty . runParser p ""

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e
