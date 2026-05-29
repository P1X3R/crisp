{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AST (
    SymbolId (..),
    Number (..),
    SExpr (..),
    runAST,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), gets, modify)
import Data.Bits (Bits (xor))
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

instance Hashable SymbolId where
    hash (SymbolId sId) = sId
    hashWithSalt salt (SymbolId sId) = salt `xor` sId -- unused

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
    case tok of
        TNumber number NTFloat -> case TR.double number of
            Right (parsed, _) -> return $ Located (SNumber $ NFloat parsed) pos
            Left _ -> throwError (LEASTError PDInvalidNumber pos)
        TNumber number NTInt -> case TR.signed TR.decimal number of
            Right (parsed, _) -> return $ Located (SNumber $ NInt parsed) pos
            Left _ -> throwError (LEASTError PDInvalidNumber pos)
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
    (ast, (ASTParserState _ idMap _)) <- runExcept $ (runParser []) (ASTParserState 0 HM.empty tokens)
    Right (ast, idMap)
  where
    runParser = runStateT . runASTParser . genAST
