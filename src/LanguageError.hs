module LanguageError (
    LangError (..),
    LexerDetail (..),
    ASTDetail (..),
    EvalDetail (..),
) where

import Location (Position)

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
    | PDInvalidNumber -- Impossible error due to Lexer validation
    | PDNoMatch
    deriving (Show, Eq)

data EvalDetail
    = EDUndefinedSymbol
    | EDArgNameAlreadyExists
    | EDWrongArgNumber
    | EDInvalidFunction
    deriving (Show, Eq)
