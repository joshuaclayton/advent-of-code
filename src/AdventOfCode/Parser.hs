module AdventOfCode.Parser
    ( signedIntegerParser
    ) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

signedIntegerParser :: Parser Integer
signedIntegerParser = L.signed spaceConsumer integer
  where
    lexeme = L.lexeme spaceConsumer
    integer = lexeme L.decimal

spaceConsumer :: Parser () -- space consumer
spaceConsumer = L.space (void $ char ' ') lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
