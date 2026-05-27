{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (spec) where

import AST (Number (..), SExpr (..), SymbolId (..), runAST)
import qualified Data.Map as M
import Hedgehog
import Lexer (NumberType (..), Token (..), runTokenizer)
import Location (Located (..), Position (..))
import ProgramHelpers (genExpr, showSExpr)
import Test.Hspec
import Test.Hspec.Hedgehog

pos :: Position
pos = Position 1 1

locate :: [a] -> [Located a]
locate l = [Located e pos | e <- l]

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
            srcFinal <- case showSExpr (unLocated astInitial) idMapInitial of
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
                    locate
                        [ TNumber "42" NTInt
                        , TBoolean True
                        , TString "hello"
                        , TEof
                        ]
            let expectedAst =
                    [ Located (SNumber (NInt 42)) pos
                    , Located (SBool True) pos
                    , Located (SStr "hello") pos
                    ]
            fst (runAST tokens) `shouldBe` expectedAst

        it "parses standard S-Expressions and tracks symbols" $ do
            let tokens =
                    locate
                        [ TLeftParen
                        , TSymbol "add"
                        , TNumber "1.5" NTFloat
                        , TNumber "2" NTInt
                        , TRightParen
                        , TEof
                        ]
            let (ast, symbolMap) = runAST tokens

            ast
                `shouldBe` [ Located
                                ( SList
                                    [ Located (SSymbol (SymbolId 0)) pos
                                    , Located (SNumber (NFloat 1.5)) pos
                                    , Located (SNumber (NInt 2)) pos
                                    ]
                                )
                                pos
                           ]
            M.lookup (SymbolId 0) symbolMap `shouldBe` Just "add"

        it "handles nested lists correctly" $ do
            -- ( ( 42 ) )
            let tokens =
                    locate
                        [ TLeftParen
                        , TLeftParen
                        , TNumber "42" NTInt
                        , TRightParen
                        , TRightParen
                        , TEof
                        ]
            let (ast, _) = runAST tokens
            ast `shouldBe` [Located (SList [Located (SList [Located (SNumber (NInt 42)) pos]) pos]) pos]

        it "parses quoted expressions" $ do
            -- '(1 2)
            let tokens =
                    locate
                        [ TQuote
                        , TLeftParen
                        , TNumber "1" NTInt
                        , TNumber "2" NTInt
                        , TRightParen
                        , TEof
                        ]
            let (ast, _) = runAST tokens
            ast `shouldBe` [Located (SQuoted (Located (SList [Located (SNumber (NInt 1)) pos, Located (SNumber (NInt 2)) pos]) pos)) pos]
