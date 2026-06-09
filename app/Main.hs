{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST (ASTParserState (..), SExpr, runAST)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import Data.Tuple (swap)
import Eval (Eval (runEval), EvalCtx (..), EvalResult (..), eval, initialEnv)
import LanguageError (ASTDetail (..), ArgNumMismatch (..), EvalDetail (..), LangError (..), LexerDetail (..), TypeMismatch (..))
import Lexer (runTokenizer)
import Location (Located, Position (..))
import Numbers (Number (..))
import Numeric (showFFloat)
import Symbols (SymbolId (SymbolId), specialSymbols)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

getErrorPos :: LangError -> Position
getErrorPos (LELexerError _ pos) = pos
getErrorPos (LEASTError _ pos) = pos
getErrorPos (LEEvalError _ pos) = pos

getErrorMsg :: LangError -> T.Text
getErrorMsg (LELexerError err _) = case err of
    LDMultipleDotInNumber -> "numbers can have at most 1 decimal dot"
    LDInvalidNumber -> "invalid number format"
    LDUnclosedString -> "found unclosed string"
    LDInvalidBool -> "boolean literals should only be `#f` or `#t`"
    LDInvalidSymbolChar -> "unallowed character"
    LDNoMatch -> "no lexer parser could parse correctly this expression"
getErrorMsg (LEASTError err _) = case err of
    PDUnclosedList -> "found `(` but without closing `)`"
    PDExtraParenthesis -> "found `)` with no opening `(`"
    PDEmptyQuote -> "found quote with no trailing expression"
    PDCriticalBug t -> "critical bug, " <> t
    PDNoMatch -> "no ast parser could parse correctly this expression"
getErrorMsg (LEEvalError err _) = case err of
    EDUndefinedSymbol (SymbolId sId) -> "undefined symbol (id: " <> T.show sId <> ")"
    EDWrongArgNumber (ArgNumMismatch funcName expected actual) ->
        "expected " <> T.show expected <> " but got " <> T.show actual <> " in function " <> fromMaybe "<unknown>" funcName
    EDNotAFunction attemptedType -> attemptedType <> " is not a valid function type"
    EDTypeMismatch (TypeMismatch expected actual) -> "expected " <> expected <> " type but got " <> actual
    EDDivisionByZero -> "attempted to divide by zero"
    EDEmptyListOperation opName -> "tried to call empty " <> opName <> " operation"

getLineAt :: Int -> T.Text -> Maybe T.Text
getLineAt n txt
    | n <= 0 = Nothing
    | otherwise = case drop (n - 1) (T.lines txt) of
        (line : _) -> Just line
        [] -> Nothing

renderErrorMsg :: T.Text -> LangError -> T.Text
renderErrorMsg src err =
    let msg = getErrorMsg err
        Position lineNum colNum = getErrorPos err
        lineText = getLineAt lineNum src
        lineNumTxt = T.show lineNum
        ptr = T.replicate (colNum - 1) " " <> "^"
        srcDisplay = case lineText of
            Nothing -> ""
            Just l -> "\n" <> lineNumTxt <> " | " <> l <> "\n" <> T.replicate (T.length lineNumTxt) " " <> " | " <> ptr
     in "error: " <> msg <> "\n" <> srcDisplay

renderEvalResult :: EvalResult -> T.Text
renderEvalResult res = case res of
    RNumber (NInt num) -> T.show num
    RNumber (NFloat num) -> T.pack $ showFFloat Nothing num ""
    RBool True -> "#t" -- Streamlined matching
    RBool False -> "#f"
    RStr str -> str
    RList list -> "(" <> T.unwords (map renderEvalResult list) <> ")"
    RPrint val -> renderEvalResult val
    _ -> ""

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
