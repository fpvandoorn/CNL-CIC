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

-- anyNameSplitAnd :: AnyName -> AnyName
-- anyNameSplitAnd


freshVariableAux :: [Text] -> Text -> Int -> Text
freshVariableAux xs x n = if x <> pack (show n) `elem` xs then freshVariableAux xs x (n+1) else x <> pack (show n)

-- generate a fresh variable.
-- todo: these might not currently be accepted as variables
freshVariable :: [Text] -> Text -> Text
freshVariable xs x = if x `elem` xs then freshVariableAux xs x 2 else x

-- add fresh variables to
-- addFreshVariables :: Statement -> Statement
-- addFreshVariables (HeadStatement )

addFreshVariables :: [Text] -> ParsedPatt -> ParsedPatt
addFreshVariables bvs (ParsedName []) := ParsedName [freshVariable bvs "x"]
addFreshVariables bvs x := x

-- data Quantifier =
--     QuantifierAll
--   | QuantifierEx
--   | QuantifierNotAll
--   | QunatifierNotEx
--   deriving (Show, Eq)


normalizeLitAny :: [Text] -> [Text]
normalizeLitAny ["every"] = ["every"]
normalizeLitAny ["each"] = ["every"]
normalizeLitAny ["each","and","every"] = ["every"]
normalizeLitAny ["all"] = ["every"]
normalizeLitAny ["any"] = ["every"]
normalizeLitAny ["some"] = ["some"]
normalizeLitAny ["no"] = ["no"]
normalizeLitAny _ = error "Unknown quantifier"

-- part of 1.4.1(3)
normalizeAnyName :: AnyName -> AnyName -- todo: recursive
normalizeAnyName (AnyNameAnyArgs q t)       = AnyNameAnyArgs (normalizeLitAny q) t
normalizeAnyName (AnyNameTypedName q t)     = AnyNameTypedName (normalizeLitAny q) t
normalizeAnyName (AnyNameFreePredicate q t) = AnyNameFreePredicate (normalizeLitAny q) t
normalizeAnyName (AnyNameGeneralType q t)   = AnyNameGeneralType (normalizeLitAny q) t

-- split and-chains of quantifiers. 1.4.1(2) in [Paskevich]
headStatementSplitAnd :: HeadStatement -> HeadStatement
headStatementSplitAnd (HeadStatementForAny (x1:x2:xs) s) =
    HeadStatementForAny [x1] $ HeadStatement $ headStatementSplitAnd $ HeadStatementForAny (x2:xs) s
headStatementSplitAnd hs = hs
