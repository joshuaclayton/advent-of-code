#!/usr/bin/env stack
-- stack --install-ghc runghc -- -isrc
import AdventOfCode
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Jump = Int

type Step = Int

data SelectedList a =
  SelectedList [a]
               a
               [a]

data Instruction =
  Instruction Step
              (SelectedList Jump)

data InstructionOutcome
  = ContinueInstruction Instruction
  | EndInstruction Instruction

buildInstruction :: SelectedList Jump -> InstructionOutcome
buildInstruction = ContinueInstruction . Instruction 0

buildInitialList :: a -> [a] -> SelectedList a
buildInitialList = SelectedList []

buildMovementFunction :: Jump -> SelectedList a -> Maybe (SelectedList a)
buildMovementFunction jump =
  if jump < 0
    then moveBackwardMultiple (abs jump)
    else moveForwardMultiple jump

currentSelection :: SelectedList a -> a
currentSelection (SelectedList _ x _) = x

mapSelected :: (a -> a) -> SelectedList a -> SelectedList a
mapSelected f (SelectedList as a bs) = SelectedList as (f a) bs

mapStep :: (Step -> Step) -> Instruction -> Instruction
mapStep f (Instruction step list) = Instruction (f step) list

runInstruction :: Instruction -> Maybe Instruction
runInstruction (Instruction steps selectedList) =
  case movement (mapSelected (+ 1) selectedList) of
    Nothing -> Nothing
    Just selectedList' -> Just $ Instruction (incrementStep steps) selectedList'
  where
    jump = currentSelection selectedList
    movement = buildMovementFunction jump

moveForwardMultiple :: Jump -> SelectedList a -> Maybe (SelectedList a)
moveForwardMultiple jump (SelectedList as a bs) =
  if jump > length bs
    then Nothing
    else Just $ SelectedList as' a' bs'
  where
    toList = as ++ [a] ++ bs
    as' = take (length as + jump) toList
    a' = toList !! (length as + jump)
    bs' = drop (length as + jump + 1) toList

moveBackwardMultiple :: Jump -> SelectedList a -> Maybe (SelectedList a)
moveBackwardMultiple jump (SelectedList as a bs) =
  if jump > length as
    then Nothing
    else Just $ SelectedList as' a' bs'
  where
    toList = as ++ [a] ++ bs
    as' = take (length as - jump) toList
    a' = toList !! (length as - jump)
    bs' = drop (length as - jump + 1) toList

processInstruction :: InstructionOutcome -> InstructionOutcome
processInstruction (EndInstruction instruction) =
  EndInstruction $ mapStep incrementStep instruction
processInstruction (ContinueInstruction instruction) =
  processInstruction $
  case runInstruction instruction of
    Nothing -> EndInstruction instruction
    Just instruction' -> ContinueInstruction instruction'

extractInstruction :: InstructionOutcome -> Instruction
extractInstruction (ContinueInstruction i) = i
extractInstruction (EndInstruction i) = i

incrementStep :: Step -> Step
incrementStep = (+) 1

main :: IO ()
main = runProgram process inputParser

steps :: Instruction -> Int
steps (Instruction i _) = i

process :: [Jump] -> Int
process [] = 0
process (x:xs) =
  steps $
  extractInstruction $
  processInstruction $ buildInstruction $ buildInitialList x xs

inputParser :: Parser [Jump]
inputParser = lineParser `sepBy` newline

lineParser :: Parser Jump
lineParser = fromIntegral <$> signedIntegerParser
