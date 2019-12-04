#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc 
--
import AdventOfCode
import Data.Char
import Data.Functor (($>))
import qualified Data.Maybe as M
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = runProgram process inputParser

type PasswordRange = (Password, Password)

process :: (Integer, Integer) -> Int
process =
    length .
    filter sameOrAscends .
    filter containsAdjacent . M.mapMaybe integerToPassword . toRange

toRange :: (Integer, Integer) -> [Integer]
toRange (a, b) = [a .. b]

data Digit
    = D0
    | D1
    | D2
    | D3
    | D4
    | D5
    | D6
    | D7
    | D8
    | D9
    deriving (Ord, Eq)

data Password =
    Password Digit
             Digit
             Digit
             Digit
             Digit
             Digit

containsAdjacent :: Password -> Bool
containsAdjacent (Password a b c d e f)
    | a == b = True
    | b == c = True
    | c == d = True
    | d == e = True
    | e == f = True
    | otherwise = False

sameOrAscends :: Password -> Bool
sameOrAscends (Password a b c d e f) =
    GT `notElem`
    [compare a b, compare b c, compare c d, compare d e, compare e f]

inputParser :: Parser (Integer, Integer)
inputParser = (,) <$> (signedIntegerParser <* char '-') <*> signedIntegerParser

integerToPassword :: Integer -> Maybe Password
integerToPassword integer =
    case map charToDigit $ show integer of
        [Just a, Just b, Just c, Just d, Just e, Just f] ->
            Just $ Password a b c d e f
        _ -> Nothing

charToDigit :: Char -> Maybe Digit
charToDigit '0' = Just D0
charToDigit '1' = Just D2
charToDigit '2' = Just D2
charToDigit '3' = Just D3
charToDigit '4' = Just D4
charToDigit '5' = Just D5
charToDigit '6' = Just D6
charToDigit '7' = Just D7
charToDigit '8' = Just D8
charToDigit '9' = Just D9
charToDigit _ = Nothing
