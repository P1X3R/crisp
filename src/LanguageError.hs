module LanguageError (
  LangError (..),
  LexerDetail (..),
) where

import Location (Position)

data LangError = LELexerError LexerDetail Position deriving (Show, Eq)

data LexerDetail
    = LDMultipleDotInNumber
    | LDInvalidNumber
    | LDUnclosedString
    | LDInvalidBool
    | LDInvalidSymbolChar
    | LDUnclosedList
    | LDExtraParenthesis
    | LDNoMatch
    deriving (Show, Eq)
