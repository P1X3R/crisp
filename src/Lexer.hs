module Lexer (
    TokenData (..),
    Token (..),
    Position (..),
    tokenize,
) where

import Control.Monad.Trans.State (StateT)
import Data.Char (isSpace)
import Data.Text as T

data TokenData
    = TLeftParen
    | TRightParen
    | TQuote
    | TNumber T.Text
    | TBoolean Bool
    | TString T.Text
    | TSymbol T.Text
    | TEof
    deriving (Show, Eq)

data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving (Show, Eq)

data Token = Token
    { tData :: TokenData
    , tPosition :: Position
    }
    deriving (Show, Eq)

data LangError
    = LEUnclosedStr
    | LEInvalidNumber
    | LEInvalidBool
    deriving (Show, Eq)

data ParserState = ParserState
    { remaining :: T.Text
    , position :: Position
    }

type ParserResult = StateT ParserState (Either LangError) Token
type Parser = T.Text -> Position -> Maybe ParserResult

advance :: Position -> Char -> Position
advance position c =
    if c == '\n'
        then Position (line position + 1) 0
        else Position (line position) (column position + 1)

consume :: Position -> T.Text -> Position
consume = T.foldl' advance

tokenize :: T.Text -> Position -> Either LangError [Token]
tokenize remaining position = case T.uncons remaining of
    Just (c, cs)
        | isSpace c -> tokenize cs (advance position c)
        | otherwise -> do
            following <- tokenize cs (advance position c)
            Right $ Token (TSymbol $ T.singleton c) position : following
    Nothing -> Right [Token TEof position]
