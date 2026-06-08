module LanguageError (
    LangError (..),
    LexerDetail (..),
    ASTDetail (..),
    ArgNumMismatch (..),
    TypeMismatch (..),
    EvalDetail (..),
) where

import qualified Data.Text as T
import Location (Position)
import Symbols (SymbolId)

data LangError
    = LELexerError LexerDetail Position
    | LEASTError ASTDetail Position
    | LEEvalError EvalDetail Position
    deriving (Show, Eq)

data LexerDetail
    = LDMultipleDotInNumber
    | LDInvalidNumber
    | LDUnclosedString
    | LDInvalidBool
    | LDInvalidSymbolChar
    | LDExtraParenthesis
    | LDNoMatch
    deriving (Show, Eq)

data ASTDetail
    = PDUnclosedList
    | PDExtraParenthesis
    | PDEmptyQuote
    | PDCriticalBug T.Text
    | PDNoMatch
    deriving (Show, Eq)

data ArgNumMismatch = ArgNumMismatch
    { anmFuncName :: Maybe T.Text -- Nothing for lambdas
    , anmExpected :: Int
    , anmActual :: Int
    }
    deriving (Show, Eq)

data TypeMismatch = TypeMismatch
    { tmExpectedType :: T.Text
    , tmActualType :: T.Text
    }
    deriving (Show, Eq)

data EvalDetail
    = EDUndefinedSymbol SymbolId
    | EDWrongArgNumber ArgNumMismatch
    | EDNotAFunction T.Text -- Just the attempted type name
    | EDTypeMismatch TypeMismatch
    | EDDivisionByZero
    | EDEmptyListOperation T.Text -- Just the operation name
    deriving (Show, Eq)
