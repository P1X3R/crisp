module LanguageError (
  LangError (..),
  LexerDetail (..),
  ASTDetail (..),
) where

import Location (Position)

data LangError = LELexerError LexerDetail Position | LEASTError ASTDetail Position deriving (Show, Eq)

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
