#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc 
--
{-# LANGUAGE OverloadedStrings #-}

import AdventOfCode
import Data.Char
import qualified Data.Maybe as M
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Program =
    Program [Intcode]
    deriving (Show)

data Intcode =
    Intcode Integer
            Integer
            Integer
            Integer
    deriving (Show)

data InstructionHandler =
    InstructionHandler Program
                       Int

main :: IO ()
main = runProgram process inputParser

process :: [Integer] -> Integer
process inputs =
    head
        [ 100 * n + v
        | n <- [0 .. 99]
        , v <- [0 .. 99]
        , process' n v inputs == 19690720
        ]

process' :: Integer -> Integer -> [Integer] -> Integer
process' noun verb =
    valueAt 0 .
    handleInstructions .
    buildInstructionHandler .
    setValueAt 2 verb .
    setValueAt 1 noun . Program . M.mapMaybe buildIntcode . inGroupsOf 4

buildIntcode :: [Integer] -> Maybe Intcode
buildIntcode [a, b, c, d] = Just $ Intcode a b c d
buildIntcode _ = Nothing

buildInstructionHandler :: Program -> InstructionHandler
buildInstructionHandler program = InstructionHandler program 0

handleInstructions :: InstructionHandler -> Program
handleInstructions (InstructionHandler program position) =
    if haltProgram intcode
        then program
        else handleInstructions $
             InstructionHandler (runInstruction program intcode) (position + 1)
  where
    intcode = intcodes program !! position

haltProgram :: Intcode -> Bool
haltProgram (Intcode 99 _ _ _) = True
haltProgram _ = False

runInstruction :: Program -> Intcode -> Program
runInstruction program intcode@(Intcode _ l r p) = setValueAt p newValue program
  where
    op = operation intcode
    newValue = valueAt l program `op` valueAt r program

processInput :: [Integer] -> Program
processInput = undefined

intcodeValue :: Intcode -> Int -> Integer
intcodeValue (Intcode a _ _ _) 0 = a
intcodeValue (Intcode _ b _ _) 1 = b
intcodeValue (Intcode _ _ c _) 2 = c
intcodeValue (Intcode _ _ _ d) 3 = d
intcodeValue _ _ = error "invalid intcodeValue position"

setIntcodeValue :: Intcode -> Int -> Integer -> Intcode
setIntcodeValue (Intcode a b c d) 0 v = Intcode v b c d
setIntcodeValue (Intcode a b c d) 1 v = Intcode a v c d
setIntcodeValue (Intcode a b c d) 2 v = Intcode a b v d
setIntcodeValue (Intcode a b c d) 3 v = Intcode a b c v
setIntcodeValue _ offset _ =
    error $ "unknown setIntcodeValue offset: " ++ show offset

operation :: Intcode -> (Integer -> Integer -> Integer)
operation (Intcode 1 _ _ _) = (+)
operation (Intcode 2 _ _ _) = (*)
operation _ = error "invalid operation lookup"

intcodes :: Program -> [Intcode]
intcodes (Program v) = v

setValueAt :: Integer -> Integer -> Program -> Program
setValueAt position newValue program =
    Program $ replace (fromIntegral offset) newIntcode (intcodes program)
  where
    newIntcode =
        setIntcodeValue
            (intcodes program !! fromIntegral offset)
            (fromIntegral positionInProgram)
            newValue
    (offset, positionInProgram) = fromIntegral position `divMod` 4

valueAt :: Integer -> Program -> Integer
valueAt position program =
    intcodeValue (intcodes program !! fromIntegral offset) positionInProgram
  where
    (offset, positionInProgram) = fromIntegral position `divMod` 4

inputParser :: Parser [Integer]
inputParser = lineValue `sepBy` char ','
  where
    lineValue = try positiveIntegerParser <|> signedIntegerParser

positiveIntegerParser :: Parser Integer
positiveIntegerParser = char '+' *> integerParser <?> "positive number"

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf 0 l = []
inGroupsOf n l = take n l : inGroupsOf n (drop n l)
