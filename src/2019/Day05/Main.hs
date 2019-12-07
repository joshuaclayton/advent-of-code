#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc 
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Data.Char
import Data.Functor (($>))
import qualified Data.Maybe as M
import Text.Megaparsec
import Text.Megaparsec.Char

data Parameter
    = Position Integer
    | Immediate Integer
    deriving (Show, Eq)

data Intcode
    = Add Parameter
          Parameter
          Integer
    | Multiply Parameter
               Parameter
               Integer
    | Input Parameter
    | Output Parameter
    | Halt
    deriving (Show, Eq)

data Program =
    Program (Integer, Maybe Integer)
            [Integer]
            Intcode
            [Integer]
    deriving (Show)

intcodeToIntegers :: Intcode -> [Integer]
intcodeToIntegers Halt = [99]
intcodeToIntegers (Input (Position p1)) = [3, p1]
intcodeToIntegers (Input (Immediate p1)) = [103, p1]
intcodeToIntegers (Output (Position p1)) = [4, p1]
intcodeToIntegers (Output (Immediate p1)) = [104, p1]
intcodeToIntegers (Add (Position p1) (Position p2) p3) = [00001, p1, p2, p3]
intcodeToIntegers (Add (Immediate p1) (Position p2) p3) = [00101, p1, p2, p3]
intcodeToIntegers (Add (Position p1) (Immediate p2) p3) = [01001, p1, p2, p3]
intcodeToIntegers (Add (Immediate p1) (Immediate p2) p3) = [01101, p1, p2, p3]
intcodeToIntegers (Multiply (Position p1) (Position p2) p3) =
    [00002, p1, p2, p3]
intcodeToIntegers (Multiply (Immediate p1) (Position p2) p3) =
    [00102, p1, p2, p3]
intcodeToIntegers (Multiply (Position p1) (Immediate p2) p3) =
    [01002, p1, p2, p3]
intcodeToIntegers (Multiply (Immediate p1) (Immediate p2) p3) =
    [01102, p1, p2, p3]

intcodeLength :: Intcode -> Int
intcodeLength Add {} = 4
intcodeLength Multiply {} = 4
intcodeLength (Input _) = 2
intcodeLength (Output _) = 2
intcodeLength Halt = 1

getValueAtPosition :: Integer -> Program -> Integer
getValueAtPosition position (Program initialValue previousIntcodes currentIntcode rest)
    | position' < positionsBeforeCurrent = previousIntcodes !! position'
    | position' >= positionsBeforeCurrent && position' < offsetAfterIntcode =
        error "getting value within current intcode"
    | otherwise = rest !! (position' - offsetAfterIntcode)
  where
    offsetAfterIntcode = positionsBeforeCurrent + intcodeLength currentIntcode
    positionsBeforeCurrent = length previousIntcodes
    position' = fromInteger position

setValueAtPosition :: Integer -> Integer -> Program -> Program
setValueAtPosition position newValue (Program initialValue previousIntcodes currentIntcode rest)
    | position' < positionsBeforeCurrent =
        Program
            initialValue
            (replace position' newValue previousIntcodes)
            currentIntcode
            rest
    | position' >= positionsBeforeCurrent && position' < offsetAfterIntcode =
        error "replacing value inside self"
    | otherwise =
        Program
            initialValue
            previousIntcodes
            currentIntcode
            (replace (position' - offsetAfterIntcode) newValue rest)
  where
    offsetAfterIntcode = positionsBeforeCurrent + intcodeLength currentIntcode
    positionsBeforeCurrent = length previousIntcodes
    position' = fromInteger position

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

initializeProgram :: Integer -> [Integer] -> Program
initializeProgram initialValue =
    until programHalts executeIntcode .
    uncurry (Program (initialValue, Nothing) []) . parseIntcode

main :: IO ()
main = runProgram process integersParser

process :: [Integer] -> Integer
process = output . initializeProgram 1

output :: Program -> Integer
output (Program (_, Just v) _ _ _) = v

integersParser :: Parser [Integer]
integersParser = signedIntegerParser `sepBy` char ','

valueFor :: Parameter -> Program -> Integer
valueFor (Immediate v) _ = v
valueFor (Position v) program = getValueAtPosition v program

setOutput :: Program -> Integer -> Program
setOutput (Program (initialValue, _) intcodes currentIntcode rest) newValue =
    Program (initialValue, Just newValue) intcodes currentIntcode rest

executeIntcode' :: Intcode -> Program -> Program
executeIntcode' intcode program@(Program initialValue intcodes currentIntcode rest) =
    case currentIntcode of
        Input (Position p1) -> setValueAtPosition p1 (fst initialValue) program
        Output p1 -> setOutput program $ valueFor p1 program
        Halt -> program
        Add p1 p2 p3 ->
            setValueAtPosition
                p3
                (valueFor p1 program + valueFor p2 program)
                program
        Multiply p1 p2 p3 ->
            setValueAtPosition
                p3
                (valueFor p1 program * valueFor p2 program)
                program

executeIntcode :: Program -> Program
executeIntcode program@(Program _ _ code _) =
    Program
        initialValue
        (intcodes ++ intcodeToIntegers currentIntcode)
        newIntcode
        rest'
  where
    (Program initialValue intcodes currentIntcode rest) =
        executeIntcode' code program
    (newIntcode, rest') = parseIntcode rest

programHalts :: Program -> Bool
programHalts (Program (_, Just _) _ Halt _) = True
programHalts _ = False

parseIntcode :: [Integer] -> (Intcode, [Integer])
parseIntcode integers =
    case integers of
        (99:rest) -> (Halt, rest)
        (3:position:rest) -> (Input $ Position position, rest)
        (103:position:rest) -> (Input $ Immediate position, rest)
        (4:position:rest) -> (Output $ Position position, rest)
        (104:position:rest) -> (Output $ Immediate position, rest)
        (n:p1:p2:p3:rest) ->
            case n of
                01101 -> (Add (Immediate p1) (Immediate p2) p3, rest)
                00101 -> (Add (Immediate p1) (Position p2) p3, rest)
                01001 -> (Add (Position p1) (Immediate p2) p3, rest)
                00001 -> (Add (Position p1) (Position p2) p3, rest)
                01102 -> (Multiply (Immediate p1) (Immediate p2) p3, rest)
                00102 -> (Multiply (Immediate p1) (Position p2) p3, rest)
                01002 -> (Multiply (Position p1) (Immediate p2) p3, rest)
                00002 -> (Multiply (Position p1) (Position p2) p3, rest)
                _ -> error $ "unexpected intcode value: " ++ show n
        _ -> error "unable to parse intcode"
