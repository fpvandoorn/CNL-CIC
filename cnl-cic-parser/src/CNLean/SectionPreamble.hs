{-
Author(s): Jesse Michael Han (2019)

Section preambles
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CNLean.SectionPreamble where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token)
import Control.Monad (guard)
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import CNLean.Basic.Basic

data SectionPreamble = SectionPreambleDummyConstructor
  deriving (Show, Eq)

parseSectionPreamble :: Parser SectionPreamble
parseSectionPreamble = empty -- not implemented yet
