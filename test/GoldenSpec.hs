{-# LANGUAGE OverloadedStrings #-}

module GoldenSpec (spec) where

import AST (runAST, initialASTState)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import Eval (EvalCtx (..), EvalResult (..), evalSExprs, initialEnv, initialEvalCtx)
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
            , goldenFile = path <> ".crisp"
            , actualFile = Just (path <> "-actual.crisp")
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
    describe "golden tests" $ do
        it "primitives" $ do
            let input = "20 3.14 #t #f \"hello\" \"world\""
            runGolden "primitives" (renderEitherResult input $ pipeline input)
