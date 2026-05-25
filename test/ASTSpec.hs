{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (spec) where

import AST (Number (..), SExpr (..), SymbolId (..), runAST)
import qualified Data.Map as M
import Hedgehog
import Lexer (NumberType (..), Position (..), Token (..), TokenData (..), runTokenizer)
import ProgramHelpers (genExpr, showSExpr)
import Test.Hspec
import Test.Hspec.Hedgehog

pos :: Position
pos = Position 1 1

spec :: Spec
spec = do
    describe "runAST" $ do
        it "property: roundtrip" $ hedgehog $ do
            srcInitial <- forAll genExpr

            -- First pass: Tokenize & Parse into AST
            tokInitial <- case runTokenizer srcInitial of
                Left err -> annotateShow err >> failure
                Right tok -> return tok
            let (astListInitial, idMapInitial) = runAST tokInitial
            astInitial <- case astListInitial of
                [ast] -> return ast
                [] -> footnote "Parser returned an empty AST list" >> failure
                _ -> footnote "Parser found multiple expressions when only one was expected" >> failure

            -- Convert AST to valid program
            srcFinal <- case showSExpr astInitial idMapInitial of
                Nothing -> do
                    footnote "showSExpr returned Nothing (Symbol ID missing from map)"
                    footnoteShow astInitial
                    failure
                Just src -> return src

            -- Second Pass: Parse the printed output
            tokFinal <- case runTokenizer srcFinal of
                Left err -> do
                    annotateShow err
                    footnote $ "Failed to tokenize printed string: " <> srcFinal
                    footnote $ "Original String: " <> srcInitial
                    footnoteShow astInitial
                    failure
                Right tok -> return tok
            let (astListFinal, _) = runAST tokFinal
            astFinal <- case astListFinal of
                [ast] -> return ast
                [] -> footnote "Second pass returned an empty AST list" >> failure
                _ -> footnote "Second pass found multiple expressions" >> failure

            footnote $ "=== ROUNDTRIP FAILURE ==="
            footnote $ "Original String: " <> srcInitial
            footnote $ "Printed String:  " <> srcFinal

            astInitial === astFinal

        it "parses flat atoms successfully" $ do
            let tokens =
                    [ Token (TNumber "42" NTInt) pos
                    , Token (TBoolean True) pos
                    , Token (TString "hello") pos
                    , Token TEof pos
                    ]
            let expectedAst =
                    [ SNumber (NInt 42)
                    , SBool True
                    , SStr "hello"
                    ]
            fst (runAST tokens) `shouldBe` expectedAst

        it "parses standard S-Expressions and tracks symbols" $ do
            let tokens =
                    [ Token TLeftParen pos
                    , Token (TSymbol "add") pos
                    , Token (TNumber "1.5" NTFloat) pos
                    , Token (TNumber "2" NTInt) pos
                    , Token TRightParen pos
                    , Token TEof pos
                    ]
            let (ast, symbolMap) = runAST tokens

            ast `shouldBe` [SList [SSymbol (SymbolId 0), SNumber (NFloat 1.5), SNumber (NInt 2)]]
            M.lookup (SymbolId 0) symbolMap `shouldBe` Just "add"

        it "handles nested lists correctly" $ do
            -- ( ( 42 ) )
            let tokens =
                    [ Token TLeftParen pos
                    , Token TLeftParen pos
                    , Token (TNumber "42" NTInt) pos
                    , Token TRightParen pos
                    , Token TRightParen pos
                    , Token TEof pos
                    ]
            let (ast, _) = runAST tokens
            ast `shouldBe` [SList [SList [SNumber (NInt 42)]]]

        it "parses quoted expressions" $ do
            -- '(1 2)
            let tokens =
                    [ Token TQuote pos
                    , Token TLeftParen pos
                    , Token (TNumber "1" NTInt) pos
                    , Token (TNumber "2" NTInt) pos
                    , Token TRightParen pos
                    , Token TEof pos
                    ]
            let (ast, _) = runAST tokens
            ast `shouldBe` [SQuoted (SList [SNumber (NInt 1), SNumber (NInt 2)])]
