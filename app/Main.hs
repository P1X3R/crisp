{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST (ASTParserState (..), SExpr, runAST)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import Data.Tuple (swap)
import Eval (Eval (runEval), EvalCtx (..), EvalResult (..), eval, initialEnv)
import LanguageError (LangError (..))
import Lexer (runTokenizer)
import Location (Located)
import Symbols (SymbolId (SymbolId), specialSymbols)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Rendering (renderErrorMsg, renderEvalResult)

evalExpr :: Located SExpr -> EvalCtx -> Either LangError EvalResult
evalExpr expr ctx = runExcept $ runReaderT (runEval (eval expr)) ctx

pipeline :: T.Text -> ASTParserState -> Either LangError ([Located SExpr], ASTParserState)
pipeline src astState = do
    tokens <- runTokenizer src
    runAST astState{aTokenStream = tokens}

consumeResults :: T.Text -> EvalCtx -> (EvalResult -> IO ()) -> [Located SExpr] -> IO EvalCtx
consumeResults _ ctx _ [] = return ctx
consumeResults src ctx printer (y : ys) = case evalExpr y ctx of
    Left err -> do
        TIO.putStrLn $ renderErrorMsg src err
        return ctx
    Right res -> do
        printer res
        let EvalCtx _ globalEnv = ctx
        let nextEnv = case res of
                RBinding key val -> HM.insert key val globalEnv
                _ -> globalEnv
        consumeResults src (EvalCtx nextEnv nextEnv) printer ys

collectInput :: Int -> T.Text -> IO (Maybe T.Text)
collectInput nestingLevel acc = do
    if nestingLevel > 0
        then TIO.putStr "....> " >> hFlush stdout
        else TIO.putStr "crisp> " >> hFlush stdout

    line <- TIO.getLine

    if T.strip line == ",q" && nestingLevel == 0
        then return Nothing -- Signal exit
        else do
            let newAcc = if T.null acc then line else acc <> "\n" <> line
                currentBalance = countParens line
                newNestingLevel = nestingLevel + currentBalance

            if newNestingLevel <= 0 && not (T.null (T.strip newAcc))
                then return (Just newAcc)
                else collectInput (max 0 newNestingLevel) newAcc
  where
    countParens :: T.Text -> Int
    countParens txt =
        let opens = T.count "(" txt
            closes = T.count ")" txt
         in opens - closes

repl :: EvalCtx -> ASTParserState -> IO ()
repl evalCtx astState = do
    maybeSrc <- collectInput 0 T.empty
    case maybeSrc of
        Nothing -> TIO.putStrLn "Bye!"
        Just src -> case pipeline src astState of
            Left err -> do
                TIO.putStrLn $ renderErrorMsg src err
                repl evalCtx astState
            Right (results, nextAstState) -> case results of
                [] -> repl evalCtx astState
                xs -> do
                    finalCtx <- consumeResults src evalCtx (TIO.putStrLn . renderEvalResult) xs
                    repl finalCtx nextAstState

runFile :: String -> IO ()
runFile path = do
    src <- TIO.readFile path
    case pipeline src initialASTState of
        Left err -> TIO.putStrLn $ renderErrorMsg src err
        Right (results, _) -> do
            _ <- consumeResults src initialEvalCtx (printOnlyOnDisplay) results
            return ()
  where
    printOnlyOnDisplay (RPrint val) = TIO.putStrLn $ renderEvalResult val
    printOnlyOnDisplay _ = return ()

initialEvalCtx :: EvalCtx
initialEvalCtx = EvalCtx initialEnv initialEnv

initialASTState :: ASTParserState
initialASTState =
    ASTParserState
        { aCurrentId = SymbolId $ length specialSymbols
        , aIdNameMap = HM.fromList (map swap specialSymbols)
        , aNameIdMap = HM.fromList specialSymbols
        , aTokenStream = []
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            TIO.putStrLn "Exit with ,q"
            repl (EvalCtx initialEnv initialEnv) initialASTState
        [path] -> runFile path
        _ -> putStrLn "Usage: crisp [file path]"
