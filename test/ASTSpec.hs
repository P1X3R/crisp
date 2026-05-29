{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (spec) where

import AST (Number (..), SExpr (..), SymbolId (..), runAST)
import qualified Data.HashMap.Strict as HM
import Hedgehog
import LanguageError (ASTDetail (..), LangError (..))
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
            (astListInitial, idMapInitial) <- evalEither $ runAST tokInitial
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
            (astListFinal, _) <- evalEither $ runAST tokFinal
            astFinal <- case astListFinal of
                [ast] -> return ast
                [] -> footnote "Second pass returned an empty AST list" >> failure
                _ -> footnote "Second pass found multiple expressions" >> failure

            footnote $ "=== ROUNDTRIP FAILURE ==="
            footnote $ "Original String: " <> srcInitial
            footnote $ "Printed String:  " <> srcFinal

            astInitial === astFinal

        it "parses flat atoms successfully" $ do
            -- 42 #t "hello"
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

            runAST tokens `shouldBe` Right (expectedAst, HM.empty)

        it "parses standard S-Expressions and tracks symbols" $ do
            -- (add 1.5 2)
            let tokens =
                    locate
                        [ TLeftParen
                        , TSymbol "add"
                        , TNumber "1.5" NTFloat
                        , TNumber "2" NTInt
                        , TRightParen
                        , TEof
                        ]
            let expectedAST =
                    [ Located
                        ( SList
                            [ Located (SSymbol (SymbolId 0)) pos
                            , Located (SNumber (NFloat 1.5)) pos
                            , Located (SNumber (NInt 2)) pos
                            ]
                        )
                        pos
                    ]
            let expectedIdMap = HM.fromList [(SymbolId 0, "add")]

            runAST tokens `shouldBe` Right (expectedAST, expectedIdMap)

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
            let expectedAST = [Located (SList [Located (SList [Located (SNumber (NInt 42)) pos]) pos]) pos]

            runAST tokens `shouldBe` Right (expectedAST, HM.empty)

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
            let expectedAST = [Located (SQuoted (Located (SList [Located (SNumber (NInt 1)) pos, Located (SNumber (NInt 2)) pos]) pos)) pos]

            runAST tokens `shouldBe` Right (expectedAST, HM.empty)

    describe "runAST error handling" $ do
        it "catches unclised lists" $ do
            -- (abc
            let tokens =
                    locate
                        [ TLeftParen
                        , TSymbol "abc"
                        , TEof
                        ]

            runAST tokens `shouldBe` Left (LEASTError PDUnclosedList pos)

        it "catches extra parenthesis" $ do
            -- ())
            let tokens =
                    locate
                        [ TLeftParen
                        , TRightParen
                        , TRightParen
                        , TEof
                        ]

            runAST tokens `shouldBe` Left (LEASTError PDExtraParenthesis pos)

        it "catches empty quote" $ do
            -- ''
            let tokens =
                    locate
                        [ TQuote
                        , TQuote
                        , TEof
                        ]

            runAST tokens `shouldBe` Left (LEASTError PDEmptyQuote pos)
