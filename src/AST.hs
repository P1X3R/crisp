{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AST (
    SExpr (..),
    ASTParserState (..),
    runAST,
    initialASTState,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import LanguageError (ASTDetail (..), LangError (..))
import Lexer (NumberType (..), Token (..))
import Location (Located (..), Position (..))
import Numbers (Number (..))
import Symbols (SymbolId (..), symQuote, specialSymbols)
import Data.Tuple (swap)

newtype ASTParser a = Parser {runASTParser :: StateT ASTParserState (Except LangError) a}
    deriving (Applicative, Functor, Monad, MonadState ASTParserState, MonadError LangError)

data ASTParserState = ASTParserState
    { aCurrentId :: SymbolId
    , aIdNameMap :: HM.HashMap SymbolId T.Text
    , aNameIdMap :: HM.HashMap T.Text SymbolId
    , aTokenStream :: [Located Token]
    }
    deriving (Show, Eq)

data SExpr
    = SNumber Number
    | SBool Bool
    | SStr T.Text
    | SSymbol SymbolId
    | SList [Located SExpr]
    deriving (Show, Eq)

instance Alternative ASTParser where
    empty = throwError (LEASTError PDNoMatch (Position 1 1))
    l <|> r =
        l `catchError` \err -> case err of
            LEASTError PDNoMatch _ -> r
            _ -> throwError err

popToken :: ASTParser (Located Token)
popToken = do
    st <- gets aTokenStream
    case st of
        [] -> empty
        (t : rest) -> do
            modify (\s -> s{aTokenStream = rest})
            return t

peekToken :: ASTParser (Located Token)
peekToken = do
    st <- gets aTokenStream
    case st of
        [] -> empty
        (t : _) -> return t

panicForNumber :: T.Text -> T.Text
panicForNumber num = "malformed number " <> num <> " reached ast parsing"

parseAtom :: ASTParser (Located SExpr)
parseAtom = do
    Located tok pos <- popToken
    expr <- case tok of
        TNumber number NTFloat -> case TR.double number of
            Right (parsed, _) -> return (SNumber $ NFloat parsed)
            Left _ -> throwError (LEASTError (PDCriticalBug $ panicForNumber number) pos)
        TNumber number NTInt -> case TR.signed TR.decimal number of
            Right (parsed, _) -> return (SNumber $ NInt parsed)
            Left _ -> throwError (LEASTError (PDCriticalBug $ panicForNumber number) pos)
        TBoolean bool -> return (SBool bool)
        TString content -> return (SStr content)
        TSymbol name -> do
            parserState <- get
            let (symbolId, newState) = storeId name parserState
            put newState
            return (SSymbol symbolId)
        _ -> empty
    return (Located expr pos)
  where
    storeId name parserState@(ASTParserState sId idToName nameToId tokens) =
        case HM.lookup name nameToId of
            Just symbolId -> (symbolId, parserState)
            Nothing ->
                ( sId
                , ASTParserState
                    { aCurrentId = sId + 1
                    , aIdNameMap = HM.insert sId name idToName
                    , aNameIdMap = HM.insert name sId nameToId
                    , aTokenStream = tokens
                    }
                )

parseList :: ASTParser (Located SExpr)
parseList = do
    Located tok pos <- popToken
    case tok of
        TLeftParen -> do
            content <- parseListContent []
            case content of
                Nothing -> throwError (LEASTError PDUnclosedList pos)
                Just c -> return $ Located (SList c) pos
        _ -> empty

parseListContent :: [Located SExpr] -> ASTParser (Maybe [Located SExpr])
parseListContent acc = do
    Located tok _ <- peekToken
    case tok of
        TRightParen -> do
            _ <- popToken -- Consume parenthesis
            return (Just $ reverse acc)
        TEof -> return Nothing -- Unclosed list
        _ -> do
            expr <- parseToken
            parseListContent (expr : acc)

parseQuote :: ASTParser (Located SExpr)
parseQuote = do
    Located tok pos <- popToken
    case tok of
        TQuote -> do
            expr <- parseToken

            -- Both the list and "quote" symbol share the same position in the source code
            return $ Located (SList [Located (SSymbol symQuote) pos, expr]) pos
        TRightParen -> throwError (LEASTError PDEmptyQuote pos)
        TEof -> throwError (LEASTError PDEmptyQuote pos)
        _ -> empty

parseToken :: ASTParser (Located SExpr)
parseToken = parseAtom <|> parseList <|> parseQuote

genAST :: [Located SExpr] -> ASTParser [Located SExpr]
genAST acc = do
    Located tok pos <- peekToken
    case tok of
        TEof -> return (reverse acc)
        TRightParen -> throwError (LEASTError PDExtraParenthesis pos)
        _ -> do
            expr <- parseToken
            genAST (expr : acc)

runAST :: ASTParserState -> Either LangError ([Located SExpr], ASTParserState)
runAST state = runExcept $ (runStateT . runASTParser . genAST) [] state

initialASTState :: [Located Token] -> ASTParserState
initialASTState tokenStream =
    ASTParserState
        { aCurrentId = SymbolId $ length specialSymbols
        , aIdNameMap = HM.fromList (map swap specialSymbols)
        , aNameIdMap = HM.fromList specialSymbols
        , aTokenStream = tokenStream
        }
