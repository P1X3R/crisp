{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AST (
    SymbolId (..),
    Number (..),
    SExpr (..),
    SpecialSymbols (..),
    runAST,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (..))
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import LanguageError (ASTDetail (..), LangError (..))
import Lexer (NumberType (..), Token (..))
import Location (Located (..), Position (Position))

newtype SymbolId = SymbolId {getId :: Int} deriving (Show, Eq, Ord, Num)

newtype ASTParser a = Parser {runASTParser :: StateT ASTParserState (Except LangError) a}
    deriving (Applicative, Functor, Monad, MonadState ASTParserState, MonadError LangError)

data ASTParserState = ASTParserState
    { aCurrentId :: SymbolId
    , aIdNameMap :: HM.HashMap SymbolId T.Text
    , aNameIdMap :: HM.HashMap T.Text SymbolId
    , aTokenStream :: [Located Token]
    }
    deriving (Show, Eq)

data Number = NFloat Double | NInt Integer deriving (Show, Eq)

data SpecialSymbols
    = PDefine
    | PIf
    | PLambda
    | PLet
    | PAdd
    | PSub
    | PMul
    | PDiv
    | PEq
    | PGreaterThan
    | PLessThan
    | PNot
    | PCons
    | PCar
    | PCdr
    | PList
    | PNull
    | PDisplay
    deriving (Show, Eq)

data SExpr
    = SNumber Number
    | SBool Bool
    | SStr T.Text
    | SSpecialSymbol SpecialSymbols
    | SSymbol SymbolId
    | SList [Located SExpr]
    | SQuoted (Located SExpr)
    deriving (Show, Eq)

instance Hashable SymbolId where
    hash (SymbolId sId) = sId
    hashWithSalt salt (SymbolId sId) = hashWithSalt salt sId

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

parseAtom :: ASTParser (Located SExpr)
parseAtom = do
    Located tok pos <- popToken
    expr <- case tok of
        TNumber number NTFloat -> case TR.double number of
            Right (parsed, _) -> return (SNumber $ NFloat parsed)
            Left _ -> throwError (LEASTError PDInvalidNumber pos)
        TNumber number NTInt -> case TR.signed TR.decimal number of
            Right (parsed, _) -> return (SNumber $ NInt parsed)
            Left _ -> throwError (LEASTError PDInvalidNumber pos)
        TBoolean bool -> return (SBool bool)
        TString content -> return (SStr content)
        TSymbol name -> case name of
            "define" -> return (SSpecialSymbol PDefine)
            "if" -> return (SSpecialSymbol PIf)
            "lambda" -> return (SSpecialSymbol PLambda)
            "let" -> return (SSpecialSymbol PLet)
            "+" -> return (SSpecialSymbol PAdd)
            "-" -> return (SSpecialSymbol PSub)
            "*" -> return (SSpecialSymbol PMul)
            "/" -> return (SSpecialSymbol PDiv)
            "=" -> return (SSpecialSymbol PEq)
            ">" -> return (SSpecialSymbol PGreaterThan)
            "<" -> return (SSpecialSymbol PLessThan)
            "not" -> return (SSpecialSymbol PNot)
            "cons" -> return (SSpecialSymbol PCons)
            "car" -> return (SSpecialSymbol PCar)
            "cdr" -> return (SSpecialSymbol PCdr)
            "list" -> return (SSpecialSymbol PList)
            "null?" -> return (SSpecialSymbol PNull)
            "display" -> return (SSpecialSymbol PDisplay)
            _ -> do
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
            return $ Located (SQuoted expr) pos
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

runAST :: [Located Token] -> Either LangError ([Located SExpr], HM.HashMap SymbolId T.Text)
runAST tokens = do
    (ast, (ASTParserState _ idMap _ _)) <- runExcept $ (runParser []) (ASTParserState 0 HM.empty HM.empty tokens)
    Right (ast, idMap)
  where
    runParser = runStateT . runASTParser . genAST
