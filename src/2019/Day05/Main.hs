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
    | JumpIfTrue Parameter
                 Parameter
    | JumpIfFalse Parameter
                  Parameter
    | LessThan Parameter
               Parameter
               Integer
    | Equals Parameter
             Parameter
             Integer
    | Halt
    deriving (Show, Eq)

data Program =
    Program (Integer, Maybe Integer)
            ![Integer]
            !Intcode
            ![Integer]
    deriving (Show)

intcodeToIntegers :: Intcode -> [Integer]
intcodeToIntegers Halt = [99]
intcodeToIntegers (Input (Position p1)) = [3, p1]
intcodeToIntegers (Input (Immediate p1)) = [103, p1]
intcodeToIntegers (Output (Position p1)) = [4, p1]
intcodeToIntegers (Output (Immediate p1)) = [104, p1]
intcodeToIntegers (JumpIfTrue (Position p1) (Position p2)) = [5, p1, p2]
intcodeToIntegers (JumpIfTrue (Position p1) (Immediate p2)) = [105, p1, p2]
intcodeToIntegers (JumpIfTrue (Immediate p1) (Position p2)) = [1005, p1, p2]
intcodeToIntegers (JumpIfTrue (Immediate p1) (Immediate p2)) = [1105, p1, p2]
intcodeToIntegers (JumpIfFalse (Position p1) (Position p2)) = [6, p1, p2]
intcodeToIntegers (JumpIfFalse (Position p1) (Immediate p2)) = [106, p1, p2]
intcodeToIntegers (JumpIfFalse (Immediate p1) (Position p2)) = [1006, p1, p2]
intcodeToIntegers (JumpIfFalse (Immediate p1) (Immediate p2)) = [1106, p1, p2]
intcodeToIntegers (LessThan (Position p1) (Position p2) p3) =
    [00007, p1, p2, p3]
intcodeToIntegers (LessThan (Immediate p1) (Position p2) p3) =
    [00107, p1, p2, p3]
intcodeToIntegers (LessThan (Position p1) (Immediate p2) p3) =
    [01007, p1, p2, p3]
intcodeToIntegers (LessThan (Immediate p1) (Immediate p2) p3) =
    [01107, p1, p2, p3]
intcodeToIntegers (Equals (Position p1) (Position p2) p3) = [00008, p1, p2, p3]
intcodeToIntegers (Equals (Immediate p1) (Position p2) p3) = [00108, p1, p2, p3]
intcodeToIntegers (Equals (Position p1) (Immediate p2) p3) = [01008, p1, p2, p3]
intcodeToIntegers (Equals (Immediate p1) (Immediate p2) p3) =
    [01108, p1, p2, p3]
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
intcodeLength JumpIfTrue {} = 3
intcodeLength JumpIfFalse {} = 3
intcodeLength Add {} = 4
intcodeLength Multiply {} = 4
intcodeLength LessThan {} = 4
intcodeLength Equals {} = 4
intcodeLength (Input _) = 2
intcodeLength (Output _) = 2
intcodeLength Halt = 1

replaceParameter :: Parameter -> Integer -> Parameter
replaceParameter (Immediate _) v = Immediate v
replaceParameter (Position _) v = Position v

changeIntcodePosition :: Intcode -> Int -> Integer -> Intcode
changeIntcodePosition Halt _ _ = error "strange"
changeIntcodePosition (Add p1 p2 p3) 1 v = Add (replaceParameter p1 v) p2 p3
changeIntcodePosition (Add p1 p2 p3) 2 v = Add p1 (replaceParameter p2 v) p3
changeIntcodePosition (Add p1 p2 p3) 3 v = Add p1 p2 v
changeIntcodePosition (Multiply p1 p2 p3) 1 v =
    Multiply (replaceParameter p1 v) p2 p3
changeIntcodePosition (Multiply p1 p2 p3) 2 v =
    Multiply p1 (replaceParameter p2 v) p3
changeIntcodePosition (Multiply p1 p2 p3) 3 v = Multiply p1 p2 v
changeIntcodePosition (LessThan p1 p2 p3) 1 v =
    LessThan (replaceParameter p1 v) p2 p3
changeIntcodePosition (LessThan p1 p2 p3) 2 v =
    LessThan p1 (replaceParameter p2 v) p3
changeIntcodePosition (LessThan p1 p2 p3) 3 v = LessThan p1 p2 v
changeIntcodePosition (Equals p1 p2 p3) 1 v =
    Equals (replaceParameter p1 v) p2 p3
changeIntcodePosition (Equals p1 p2 p3) 2 v =
    Equals p1 (replaceParameter p2 v) p3
changeIntcodePosition (Equals p1 p2 p3) 3 v = Equals p1 p2 v
changeIntcodePosition (Input p1) 1 v = Input (replaceParameter p1 v)
changeIntcodePosition (Output p1) 1 v = Output (replaceParameter p1 v)

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
    | position' == positionsBeforeCurrent =
        error "changing the entire type of intcode"
    | position' > positionsBeforeCurrent && position' < offsetAfterIntcode =
        Program
            initialValue
            previousIntcodes
            (changeIntcodePosition
                 currentIntcode
                 (position' - positionsBeforeCurrent)
                 newValue)
            rest
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

starting = 5

process :: [Integer] -> Integer
process = output . initializeProgram starting

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

setInstructionPointerTo :: Integer -> Program -> Program
setInstructionPointerTo position (Program initialValue intcodes currentIntcode rest) =
    Program initialValue intcodes' currentIntcode' rest'
  where
    programToIntegers = intcodes ++ intcodeToIntegers currentIntcode ++ rest
    (intcodes', remainder) = splitAt position' programToIntegers
    (currentIntcode', rest') = parseIntcode remainder
    position' = fromInteger position

executeIntcode' :: Intcode -> Program -> (Program, Bool)
executeIntcode' intcode program@(Program initialValue intcodes currentIntcode rest) =
    case intcode of
        Input (Position p1) ->
            (setValueAtPosition p1 (fst initialValue) program, True)
        Output p1 -> (setOutput program $ valueFor p1 program, True)
        Halt -> (program, True)
        Add p1 p2 p3 ->
            ( setValueAtPosition
                  p3
                  (valueFor p1 program + valueFor p2 program)
                  program
            , True)
        Multiply p1 p2 p3 ->
            ( setValueAtPosition
                  p3
                  (valueFor p1 program * valueFor p2 program)
                  program
            , True)
        JumpIfTrue p1 p2 ->
            if valueFor p1 program /= 0
                then ( setInstructionPointerTo (valueFor p2 program) program
                     , False)
                else (program, True)
        JumpIfFalse p1 p2 ->
            if valueFor p1 program == 0
                then ( setInstructionPointerTo (valueFor p2 program) program
                     , False)
                else (program, True)
        LessThan p1 p2 p3 ->
            ( if valueFor p1 program < valueFor p2 program
                  then setValueAtPosition p3 1 program
                  else setValueAtPosition p3 0 program
            , True)
        Equals p1 p2 p3 ->
            ( if valueFor p1 program == valueFor p2 program
                  then setValueAtPosition p3 1 program
                  else setValueAtPosition p3 0 program
            , True)

executeIntcode :: Program -> Program
executeIntcode program@(Program _ _ code _) =
    t "new program" $
    Program
        initialValue
        (intcodes ++ intcodeToIntegers currentIntcode)
        newIntcode
        rest'
  where
    ((Program initialValue intcodes currentIntcode rest), continue) =
        executeIntcode' code program
    (newIntcode, rest') =
        case (continue, rest) of
            (True, []) -> (currentIntcode, rest)
            (True, _) -> parseIntcode rest
            (False, _) -> (currentIntcode, rest)

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
                00005 -> (JumpIfTrue (Position p1) (Position p2), p3 : rest)
                00105 -> (JumpIfTrue (Position p1) (Immediate p2), p3 : rest)
                01005 -> (JumpIfTrue (Immediate p1) (Position p2), p3 : rest)
                01105 -> (JumpIfTrue (Immediate p1) (Immediate p2), p3 : rest)
                00006 -> (JumpIfFalse (Position p1) (Position p2), p3 : rest)
                00106 -> (JumpIfFalse (Position p1) (Immediate p2), p3 : rest)
                01006 -> (JumpIfFalse (Immediate p1) (Position p2), p3 : rest)
                01106 -> (JumpIfFalse (Immediate p1) (Immediate p2), p3 : rest)
                01107 -> (LessThan (Immediate p1) (Immediate p2) p3, rest)
                00107 -> (LessThan (Immediate p1) (Position p2) p3, rest)
                01007 -> (LessThan (Position p1) (Immediate p2) p3, rest)
                00007 -> (LessThan (Position p1) (Position p2) p3, rest)
                01108 -> (Equals (Immediate p1) (Immediate p2) p3, rest)
                00108 -> (Equals (Immediate p1) (Position p2) p3, rest)
                01008 -> (Equals (Position p1) (Immediate p2) p3, rest)
                00008 -> (Equals (Position p1) (Position p2) p3, rest)
                _ -> error $ "unexpected intcode value: " ++ show n
        _ -> error $ "unable to parse intcode: " ++ show integers
