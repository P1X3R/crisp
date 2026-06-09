{-# LANGUAGE OverloadedStrings #-}

module Rendering (
    renderErrorMsg,
    renderEvalResult,
) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Eval (EvalResult (..))
import LanguageError (ASTDetail (..), ArgNumMismatch (..), EvalDetail (..), LangError (..), LexerDetail (..), TypeMismatch (..))
import Location (Position (..))
import Numbers (Number (..))
import Numeric (showFFloat)
import Symbols (SymbolId (SymbolId))

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
    RBool True -> "#t"
    RBool False -> "#f"
    RStr str -> str
    RSymbol (SymbolId sId) -> "<symbol id: " <> T.show sId <> ">"
    RList list -> "(" <> T.unwords (map renderEvalResult list) <> ")"
    RBinding (SymbolId sId) val -> "<binding: id " <> T.show sId <> " -> " <> renderEvalResult val <> ">"
    RPrint val -> renderEvalResult val
    RSpecialForm _ -> "<special form>"
    RPrimitive _ -> "<primitive>"
    RClosure _ _ _ -> "<closure>"
