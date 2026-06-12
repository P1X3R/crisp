{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST (ASTParserState (..), SExpr, initialASTState, runAST)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import Eval (EvalCtx (..), EvalResult (..), evalSExprs, initialEvalCtx)
import LanguageError (LangError (..))
import Lexer (runTokenizer)
import Location (Located)
import Rendering (renderErrorMsg, renderEvalResult)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

pipeline :: T.Text -> ASTParserState -> Either LangError ([Located SExpr], ASTParserState)
pipeline src astState = do
    tokens <- runTokenizer src
    runAST astState{aTokenStream = tokens}

consumeResults :: T.Text -> EvalCtx -> (EvalResult -> IO ()) -> [Located SExpr] -> IO EvalCtx
consumeResults src evalCtx printer exprs = do
    let (evalResults, finalEvalCtx) = evalSExprs exprs evalCtx
    mapM_ render evalResults
    return finalEvalCtx
    where
        render (Left err) = TIO.putStrLn $ renderErrorMsg src err
        render (Right res) = printer res

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
    case pipeline src (initialASTState []) of
        Left err -> TIO.putStrLn $ renderErrorMsg src err
        Right (results, _) -> do
            _ <- consumeResults src initialEvalCtx (printOnlyOnDisplay) results
            return ()
  where
    printOnlyOnDisplay (RPrint val) = TIO.putStrLn $ renderEvalResult val
    printOnlyOnDisplay _ = return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            TIO.putStrLn "Exit with ,q"
            repl initialEvalCtx (initialASTState [])
        [path] -> runFile path
        _ -> putStrLn "Usage: crisp [file path]"
