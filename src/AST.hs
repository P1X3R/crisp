{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AST (
    SymbolId (..),
    Number (..),
    SExpr (..),
    runAST,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.State.Strict (MonadState, StateT (runStateT), gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Lexer (NumberType (..), Token (..))
import Location (Located (..))

newtype SymbolId = SymbolId {getId :: Int} deriving (Show, Eq, Ord, Num)

newtype ASTParser a = Parser {runASTParser :: StateT ASTParserState Maybe a}
    deriving (Applicative, Functor, Monad, MonadState ASTParserState, Alternative)

data ASTParserState = ASTParserState
    { aCurrentId :: SymbolId
    , aIdNameMap :: M.Map SymbolId T.Text
    , aTokenStream :: [Located Token]
    }
    deriving (Show, Eq)

data Number = NFloat Double | NInt Integer deriving (Show, Eq)

data SExpr
    = SNumber Number
    | SBool Bool
    | SStr T.Text
    | SSymbol SymbolId
    | SList [Located SExpr]
    | SQuoted (Located SExpr)
    deriving (Show, Eq)

throwFatalToken :: String -> String -> a
throwFatalToken msg token = error ("Parsing token " ++ token ++ ": " ++ msg)

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
    case tok of
        TNumber number NTFloat -> case TR.double number of
            Right (parsed, _) -> return $ Located (SNumber $ NFloat parsed) pos
            Left e -> throwFatalToken e (T.unpack number)
        TNumber number NTInt -> case TR.signed TR.decimal number of
            Right (parsed, _) -> return $ Located (SNumber $ NInt parsed) pos
            Left e -> throwFatalToken e (T.unpack number)
        TBoolean bool -> return $ Located (SBool bool) pos
        TString content -> return $ Located (SStr content) pos
        TSymbol name -> do
            symbolId <- gets aCurrentId
            modify (storeId name)
            return $ Located (SSymbol symbolId) pos
        _ -> empty
  where
    storeId name (ASTParserState sId idToM tokens) =
        ASTParserState (sId + 1) (M.insert sId name idToM) tokens

parseList :: ASTParser (Located SExpr)
parseList = do
    Located tok pos <- popToken
    case tok of
        TLeftParen -> do
            content <- parseListContent []
            return $ Located (SList content) pos
        _ -> empty

parseListContent :: [Located SExpr] -> ASTParser [Located SExpr]
parseListContent acc = do
    Located tok _ <- peekToken
    case tok of
        TRightParen -> do
            _ <- popToken -- Consume parenthesis
            return (reverse acc)
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
        _ -> empty

parseToken :: ASTParser (Located SExpr)
parseToken = parseAtom <|> parseList <|> parseQuote

genAST :: [Located SExpr] -> ASTParser [Located SExpr]
genAST acc = do
    Located tok _ <- peekToken
    case tok of
        TEof -> return (reverse acc)
        _ -> do
            expr <- parseToken
            genAST (expr : acc)

runAST :: [Located Token] -> ([Located SExpr], M.Map SymbolId T.Text)
runAST tokens = case (runParser []) (ASTParserState 0 M.empty tokens) of
    Nothing -> error "fatal: Parsing failed structural validation"
    Just (ast, (ASTParserState _ idMap _)) -> (ast, idMap)
  where
    runParser = runStateT . runASTParser . genAST
