{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-
Author(s): Floris van Doorn (2019)

Experimental file to transform parsed expressions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.Transformations.Transformation where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')
import Control.Lens

-- import Colada.Basic.Basic
-- import Colada.PhraseList
-- import Colada.Pattern
import Colada.Type

anyNameSplitAnd :: AnyName -> AnyName
anyNameSplitAnd

headStatementSplitAnd :: HeadStatement -> HeadStatement
headStatementSplitAnd (HeadStatementForAny (x1:x2:xs) s) = HeadStatementForAny (x2:x1:xs) s
headStatementSplitAnd hs = hs
