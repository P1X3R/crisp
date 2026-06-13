{-# LANGUAGE OverloadedStrings #-}

module GoldenSpec (spec) where

import AST (initialASTState, runAST)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import Eval (EvalResult (..), evalSExprs, initialEvalCtx)
import LanguageError (LangError)
import Lexer (runTokenizer)
import Rendering (renderErrorMsg, renderEvalResult)
import Test.Hspec
import Test.Hspec.Golden

renderEitherResult :: T.Text -> Either LangError [EvalResult] -> T.Text
renderEitherResult src langRes = case langRes of
    Left err -> renderErrorMsg src err
    Right evalRes -> T.unlines $ map renderEvalResult evalRes

runGolden :: String -> T.Text -> Golden T.Text
runGolden name actualOutput =
    let path = "test/golden/" <> name
     in Golden
            { output = actualOutput
            , encodePretty = T.unpack
            , writeToFile = TIO.writeFile
            , readFromFile = TIO.readFile
            , goldenFile = path
            , actualFile = Just (path <> "-actual")
            , failFirstTime = False
            }

pipeline :: T.Text -> Either LangError [EvalResult]
pipeline src = do
    tokens <- runTokenizer src
    (ast, _) <- runAST (initialASTState tokens)
    (evalRes, _) <- evalSExprs ast initialEvalCtx
    Right evalRes

spec :: Spec
spec = do
    describe "environment and scoping" $ do
        it "`define` mutates context for all following expressions" $ do
            let input =
                    T.unlines
                        [ "(define x 1)"
                        , "(define x (+ x x))" -- Should be 2
                        , "(define x (+ x x))" -- Should be 4
                        ]
            runGolden "define" (renderEitherResult input $ pipeline input)

        it "`lambda` uses lexical scoping" $ do
            let input =
                    T.unlines
                        [ "(define a 1)"
                        , "(define fn (lambda () (+ 1 a)))"
                        , "(define a 2)"
                        , "(fn)" -- Should be 2
                        ]
            runGolden "lambda" (renderEitherResult input $ pipeline input)

        it "`let` behaves like `let*` from Racket" $ do
            let input =
                    T.unlines
                        [ "(define x 0)"
                        , "(let ((x 1) (y (+ x 1))) y)" -- Should be 2
                        ]
            runGolden "let-let*" (renderEitherResult input $ pipeline input)

        it "`let` shadows variables only inside its internal environment" $ do
            let input =
                    T.unlines
                        [ "(define x 10)"
                        , "(let ((x 2)) x)"
                        , "x" -- Should be 10
                        ]
            runGolden "let-shadowing" (renderEitherResult input $ pipeline input)

    describe "control flow" $ do
        it "`quote` prevents evaluation" $ do
            let input = "'(+ 1 (+ 1 (+ 1)))" -- Should be just the same list
            runGolden "quote" (renderEitherResult input $ pipeline input)

        it "`if` short circuits" $ do
            -- Should not fail
            let input =
                    T.unlines
                        [ "(if #t 42 (unbound-variable-should-be-ignored))"
                        , "(if #f (unbound-variable-should-be-ignored) 24)"
                        ]
            runGolden "if" (renderEitherResult input $ pipeline input)

    it "list operations" $ do
        let input =
                T.unlines
                    [ "(cons 1 (list 2 3))"
                    , "(car (cdr (list 1 2 3)))"
                    , "(equal? (list 1 (list 2 3)) (list 1 (list 2 3)))"
                    , "(equal? (list 1 2) (list 1 3))"
                    , "(null? (quote ()))"
                    ]
        runGolden "lists-ops" (renderEitherResult input $ pipeline input)

    describe "error reporting" $ do
        it "error type mismatch" $ do
            let input = "(+ 1 \"not a number\")"
            runGolden "errors/type_mismatch" (renderEitherResult input $ pipeline input)

        it "error division by zero" $ do
            let input = "(/ 5 0)"
            runGolden "errors/division_by_zero" (renderEitherResult input $ pipeline input)

        it "error empty list operation" $ do
            let input = "(car (quote ()))"
            runGolden "errors/empty_list" (renderEitherResult input $ pipeline input)

        it "error lambda syntax mismatch" $ do
            let input = "(lambda not-a-list 42)"
            runGolden "errors/lambda_syntax" (renderEitherResult input $ pipeline input)

        it "error define syntax mismatch" $ do
            let input = "(define 123 456)"
            runGolden "errors/define_syntax" (renderEitherResult input $ pipeline input)

        it "error undefined symbol" $ do
            let input = "(+ total-amount 10)"
            runGolden "errors/undefined_symbol" (renderEitherResult input $ pipeline input)

        it "error argument count mismatch" $ do
            let input = "(if #t 1 2 3 4)"
            runGolden "errors/arg_count_mismatch" (renderEitherResult input $ pipeline input)
