{-# LANGUAGE NamedFieldPuns #-}

module Lexer (
    TokenData (..),
    Token (..),
    Position (..),
    tokenize,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State (StateT, get, gets, modify)
import Data.Char (isAlpha, isDigit)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as B

data TokenData
    = TLeftParen
    | TRightParen
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
    | LDUnclosedString
    | LDInvalidBool
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

expectParser :: (Char -> Bool) -> Parser ()
expectParser predicate = do
    rest <- gets pRest
    case T.uncons rest of
        Just (c, _) | predicate c -> pure ()
        _ -> throwError (LELexerError LDNoMatch)

isEndOfFile :: Parser Bool
isEndOfFile = do
    rest <- gets pRest
    return $ T.null rest

consume :: (Char -> Bool) -> B.Builder -> Parser B.Builder
consume predicate acc = do
    rest <- gets pRest
    case T.uncons rest of
        Just (c, _) | predicate c -> do
            modify advance
            consume predicate (acc <> B.singleton c)
        _ -> return acc

parseLeftParen :: Parser Token
parseLeftParen = do
    expectParser (== '(')

    pos <- gets pPos
    modify advance
    return $ Token TLeftParen pos

parseRightParen :: Parser Token
parseRightParen = do
    expectParser (== ')')

    pos <- gets pPos
    modify advance
    return $ Token TRightParen pos

parseNumber :: Parser Token
parseNumber = do
    expectParser (\c -> isDigit c || c == '-')

    pos <- gets pPos
    numberBuilder <- consumeNumber False mempty
    let number = toStrict $ B.toLazyText numberBuilder
    case T.unsnoc number of
        Just (_, lst) | isDigit lst -> return $ Token (TNumber number) pos
        _ -> throwError (LELexerError LDInvalidNumber)
  where
    consumeNumber :: Bool -> B.Builder -> Parser B.Builder
    consumeNumber sawDot acc = do
        rest <- gets pRest
        case T.uncons rest of
            Just (c, _) | isDigit c -> do
                modify advance
                consumeNumber sawDot $ acc <> B.singleton c
            Just ('-', _) | acc == mempty -> do
                modify advance
                consumeNumber False $ acc <> B.singleton '-'
            Just ('.', _) | not sawDot -> do
                modify advance
                consumeNumber True $ acc <> B.singleton '.'
            Just ('.', _) | sawDot -> throwError (LELexerError LDMultipleDotInNumber)
            _ -> return acc

parseBool :: Parser Token
parseBool = do
    expectParser (== '#')
    modify advance

    ParserState{pRest = rest, pPos = pos} <- get
    value <- case T.uncons rest of
        Just ('t', _) -> return True
        Just ('f', _) -> return False
        _ -> throwError (LELexerError LDInvalidBool)

    return $ Token (TBoolean value) pos

parseString :: Parser Token
parseString = do
    expectParser (== '"')
    modify advance

    contentBuilder <- consume (/= '"') mempty
    let content = toStrict $ B.toLazyText contentBuilder

    isEof <- isEndOfFile
    pos <- gets pPos
    if isEof
        then throwError (LELexerError LDUnclosedString)
        else return $ Token (TString content) pos

parseSymbol :: Parser Token
parseSymbol = do
    expectParser isSymbolValidChar

    contentBuilder <- consume isSymbolValidChar mempty
    let content = toStrict $ B.toLazyText contentBuilder

    pos <- gets pPos
    return $ Token (TSymbol content) pos
  where
    isSymbolValidChar c = c `elem` ['+', '-', '*', '/', '>', '<', '=', '!', '?', '_'] || isAlpha c

tokenize :: T.Text -> Either LangError [Token]
tokenize code = undefined
