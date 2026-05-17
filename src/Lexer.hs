{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer (
    TokenData (..),
    Token (..),
    Position (..),
    Parser (..),
    ParserState (..),
    LangError (..),
    LexerDetail (..),
    runTokenizer,
) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.State (MonadState, get, gets, modify)
import Control.Monad.Trans.Except (Except, runExcept)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Char (isAlpha, isDigit, isSpace)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as B

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
    deriving (Show, Eq, Ord)

data Token = Token
    { tData :: TokenData
    , tPosition :: Position
    }
    deriving (Show, Eq)

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

data ParserState = ParserState
    { pRest :: T.Text
    , pPos :: Position
    , pListStack :: [Position]
    }

newtype Parser a = Parser {runParser :: StateT ParserState (Except LangError) a}
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError LangError)

instance Alternative Parser where
    empty = noMatch
    l <|> r =
        l `catchError` \err -> case err of
            LELexerError LDNoMatch _ -> r
            _ -> throwError err

throwLexError :: LexerDetail -> Parser a
throwLexError detail = do
    pos <- gets pPos
    throwError (LELexerError detail pos)

noMatch :: Parser a
noMatch = throwLexError LDNoMatch

advance :: ParserState -> ParserState
advance state@ParserState{pRest, pPos, pListStack} = case T.uncons pRest of
    Just (c, cs) -> ParserState{pRest = cs, pPos = movePosition pPos c, pListStack}
    Nothing -> state
  where
    movePosition (Position l _) '\n' = Position (l + 1) 1
    movePosition (Position l col) _ = Position l (col + 1)

expectParser :: (Char -> Bool) -> Parser ()
expectParser predicate = do
    rest <- gets pRest
    case T.uncons rest of
        Just (c, _) | predicate c -> pure ()
        _ -> noMatch

reservedKeywords :: M.Map T.Text TokenData
reservedKeywords = M.fromList [("quote", TQuote)]

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
    modify $ \state ->
        state
            { pListStack = pos : pListStack state
            }

    modify advance
    return $ Token TLeftParen pos

parseRightParen :: Parser Token
parseRightParen = do
    expectParser (== ')')

    positions <- gets pListStack
    case positions of
        [] -> throwLexError LDExtraParenthesis
        (_ : ps) -> modify $ \state -> state{pListStack = ps}

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
        _ | number == "-" -> noMatch
        _ -> throwLexError LDInvalidNumber
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
            Just ('.', _) | sawDot -> throwLexError LDMultipleDotInNumber
            _ -> return acc

parseBool :: Parser Token
parseBool = do
    expectParser (== '#')

    modify advance -- Consume #
    ParserState{pRest = rest, pPos = pos} <- get

    value <- case T.uncons rest of
        Just ('t', _) -> return True
        Just ('f', _) -> return False
        _ -> throwLexError LDInvalidBool

    modify advance -- Consume t/f
    return $ Token (TBoolean value) pos

parseString :: Parser Token
parseString = do
    expectParser (== '"')
    pos <- gets pPos

    modify advance
    contentBuilder <- consume (/= '"') mempty
    let content = toStrict $ B.toLazyText contentBuilder

    isEof <- isEndOfFile
    if isEof
        then throwLexError LDUnclosedString
        else do
            modify advance
            return $ Token (TString content) pos

parseSymbol :: Parser Token
parseSymbol = do
    expectParser isSymbolStartChar
    pos <- gets pPos

    contentBuilder <- consume isSymbolBodyChar mempty
    let content = toStrict $ B.toLazyText contentBuilder

    let tokenType = case M.lookup content reservedKeywords of
            Just t -> t
            Nothing -> TSymbol content

    return $ Token tokenType pos
  where
    isSymbolStartChar c = c `elem` ['+', '-', '*', '/', '>', '<', '=', '!', '?', '_'] || isAlpha c
    isSymbolBodyChar c = isSymbolStartChar c || isDigit c

parseQuote :: Parser Token
parseQuote = do
    expectParser (== '\'')
    pos <- gets pPos
    modify advance
    return $ Token TQuote pos

parsers :: Parser Token
parsers =
    parseLeftParen
        <|> parseRightParen
        <|> parseQuote
        <|> parseNumber
        <|> parseBool
        <|> parseString
        <|> parseSymbol
        <|> throwLexError LDInvalidSymbolChar

tokenize :: [Token] -> Parser [Token]
tokenize acc = do
    rest <- gets pRest
    case T.uncons rest of
        Just (';', _) -> do
            _ <- consume (/= '\n') mempty
            tokenize acc
        Just (c, _) | isSpace c -> do
            modify advance
            tokenize acc
        Nothing -> do
            positions <- gets pListStack
            pos <- gets pPos
            case positions of
                [] -> return $ Token TEof pos : acc
                (frst : _) -> throwError (LELexerError LDUnclosedList frst)
        _ -> do
            token <- parsers
            tokenize $ token : acc

runTokenizer :: String -> Either LangError [Token]
runTokenizer input =
    let initialState =
            ParserState
                { pRest = T.pack input
                , pPos = Position{pLine = 1, pColumn = 1}
                , pListStack = []
                }
     in case runExcept (runStateT (runParser $ tokenize []) initialState) of
            Left err -> Left err
            Right (tokens, _) -> Right (reverse tokens)
