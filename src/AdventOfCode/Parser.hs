module AdventOfCode.Parser
  ( Parser
  , signedIntegerParser
  , integerParser
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Text.Megaparsec.Parsec Void Text

integerParser :: Parser Integer
integerParser = lexeme L.decimal

signedIntegerParser :: Parser Integer
signedIntegerParser = L.signed spaceConsumer integerParser

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ char ' ') lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
