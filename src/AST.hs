{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AST (
    SymbolId,
    Number (..),
    SExpr (..),
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Lexer (NumberType (..), Token (..), TokenData (..))

newtype SymbolId = SymbolId {getId :: Int} deriving (Show, Eq, Ord, Num)

newtype ASTParser a = Parser {runASTParser :: StateT ASTParserState Maybe a}
    deriving (Applicative, Functor, Monad, MonadState ASTParserState, Alternative)

data ASTParserState = ASTParserState
    { aCurrentId :: SymbolId
    , aIdNameMap :: M.Map SymbolId T.Text
    , aTokenStream :: [Token]
    }
    deriving (Show, Eq)

data Number = NFloat Double | NInt Integer deriving (Show, Eq)

data SExpr
    = SNumber Number
    | SBool Bool
    | SStr T.Text
    | SSymbol SymbolId
    | SList [SExpr]
    | SQuoted SExpr
    deriving (Show, Eq)
