{-# LANGUAGE NamedFieldPuns #-}

module Lexer (
    TokenData (..),
    Token (..),
    Position (..),
    tokenize,
) where

import Control.Monad.Trans.State (StateT)
import Data.Char (isSpace)
import Data.Text as T
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State (StateT, gets, modify)
import qualified Data.Text as T

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
    { pLine :: Int
    , pColumn :: Int
    }
    deriving (Show, Eq)

data Token = Token
    { tData :: TokenData
    , tPosition :: Position
    }
    deriving (Show, Eq)

data LangError = LELexerError LexerDetail deriving (Show, Eq)

data LexerDetail
    = LDMultipleDotInNumber
    | LDInvalidNumber
    | LDUnexpectedPeek
    | LDNoMatch
    deriving (Show, Eq)

data ParserState = ParserState
    { pRest :: T.Text
    , pPos :: Position
    }

type Parser a = StateT ParserState (Except LangError) a

advance :: ParserState -> ParserState
advance state@ParserState{pRest, pPos} = case T.uncons pRest of
    Just (c, cs) -> ParserState{pRest = cs, pPos = movePosition pPos c}
    Nothing -> state
  where
    movePosition (Position l _) '\n' = Position (l + 1) 1
    movePosition (Position l col) _ = Position l (col + 1)

peek :: Parser Char
peek = do
    rest <- gets pRest
    case T.uncons rest of
        Just (c, _) -> return c
        Nothing -> throwError (LELexerError LDUnexpectedPeek)


tokenize :: T.Text -> Either LangError [Token]
tokenize code = undefined
