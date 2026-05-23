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

throwFatalToken :: String -> String -> a
throwFatalToken msg token = error ("Parsing token " ++ token ++ ": " ++ msg)

popToken :: ASTParser TokenData
popToken = do
    ASTParserState currId idMap st <- get
    case st of
        [] -> empty
        (Token t _ : rest) -> do
            put (ASTParserState currId idMap rest)
            return t

peekToken :: ASTParser TokenData
peekToken = do
    st <- gets aTokenStream
    case st of
        [] -> empty
        (Token t _ : _) -> return t
