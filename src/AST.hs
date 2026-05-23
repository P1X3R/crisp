{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AST (
    SymbolId,
    Number (..),
    SExpr (..),
    runAST,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Lexer (NumberType (..), Token (..), TokenData (..))

newtype SymbolId = SymbolId {getId :: Int} deriving (Show, Eq, Ord, Num)

newtype ASTParser a = Parser {runASTParser :: StateT ASTParserState Maybe a}
    deriving (Applicative, Functor, Monad, MonadState ASTParserState, Alternative)

data ASTParserState = ASTParserState
    { aCurrentId :: SymbolId
    , aIdNameMap :: M.Map SymbolId T.Text
    , aTokenStream :: [Token]
    }
    deriving (Show, Eq)

data Number = NFloat Double | NInt Integer deriving (Show, Eq)

data SExpr
    = SNumber Number
    | SBool Bool
    | SStr T.Text
    | SSymbol SymbolId
    | SList [SExpr]
    | SQuoted SExpr
    deriving (Show, Eq)

throwFatalToken :: String -> String -> a
throwFatalToken msg token = error ("Parsing token " ++ token ++ ": " ++ msg)

popToken :: ASTParser TokenData
popToken = do
    ASTParserState currId idMap st <- get
    case st of
        [] -> empty
        (Token t _ : rest) -> do
            put (ASTParserState currId idMap rest)
            return t

peekToken :: ASTParser TokenData
peekToken = do
    st <- gets aTokenStream
    case st of
        [] -> empty
        (Token t _ : _) -> return t

parseAtom :: ASTParser SExpr
parseAtom = do
    tok <- popToken
    case tok of
        TNumber number NTFloat -> case TR.double number of
            Right (parsed, _) -> return (SNumber $ NFloat parsed)
            Left e -> throwFatalToken e (T.unpack number)
        TNumber number NTInt -> case TR.signed TR.decimal number of
            Right (parsed, _) -> return (SNumber $ NInt parsed)
            Left e -> throwFatalToken e (T.unpack number)
        TBoolean bool -> return (SBool bool)
        TString content -> return (SStr content)
        TSymbol name -> do
            ASTParserState symbolId _ _ <- get
            modify (storeId name)
            return (SSymbol symbolId)
        _ -> empty
  where
    storeId name (ASTParserState sId idToM tokens) =
        ASTParserState (sId + 1) (M.insert sId name idToM) tokens

parseList :: ASTParser SExpr
parseList = do
    tok <- popToken
    case tok of
        TLeftParen -> do
            content <- parseListContent []
            return (SList content)
        _ -> empty

parseListContent :: [SExpr] -> ASTParser [SExpr]
parseListContent acc = do
    tok <- peekToken
    case tok of
        TRightParen -> do
            _ <- popToken -- Consume parenthesis
            return (reverse acc)
        _ -> do
            expr <- parseToken
            parseListContent (expr : acc)

parseQuote :: ASTParser SExpr
parseQuote = do
    tok <- popToken
    case tok of
        TQuote -> do
            expr <- parseToken
            return (SQuoted expr)
        _ -> empty

parseToken :: ASTParser SExpr
parseToken = parseAtom <|> parseList <|> parseQuote

genAST :: [SExpr] -> ASTParser [SExpr]
genAST acc = do
    tok <- peekToken
    case tok of
        TEof -> return (reverse acc)
        _ -> do
            expr <- parseToken
            genAST (expr : acc)

runAST :: [Token] -> ([SExpr], M.Map SymbolId T.Text)
runAST tokens = case (runParser []) (ASTParserState 0 M.empty tokens) of
    Nothing -> error "fatal"
    Just (ast, (ASTParserState _ idMap _)) -> (ast, idMap)
  where
    runParser = runStateT . runASTParser . genAST
