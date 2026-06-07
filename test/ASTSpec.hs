{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (spec) where

import AST (SExpr (..), runAST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Tuple (swap)
import Hedgehog
import LanguageError (ASTDetail (..), LangError (..))
import Lexer (NumberType (..), Token (..), runTokenizer)
import Location (Located (..), Position (..))
import Numbers (Number (..))
import ProgramHelpers (genExpr, showSExpr)
import Symbols (SymbolId (..), specialSymbols, symQuote)
import Test.Hspec
import Test.Hspec.Hedgehog

pos :: Position
pos = Position 1 1

locate :: [a] -> [Located a]
locate l = [Located e pos | e <- l]

-- Helper to construct the expected environment map alongside any dynamic identifiers
buildExpectedMap :: [(SymbolId, T.Text)] -> HM.HashMap SymbolId T.Text
buildExpectedMap dynamics = HM.union (HM.fromList dynamics) (HM.fromList $ map swap specialSymbols)

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
                    footnote $ "Failed to tokenize printed string: " <> T.unpack srcFinal
                    footnote $ "Original String: " <> T.unpack srcInitial
                    footnoteShow astInitial
                    failure
                Right tok -> return tok
            (astListFinal, _) <- evalEither $ runAST tokFinal
            astFinal <- case astListFinal of
                [ast] -> return ast
                [] -> footnote "Second pass returned an empty AST list" >> failure
                _ -> footnote "Second pass found multiple expressions" >> failure

            footnote $ "=== ROUNDTRIP FAILURE ==="
            footnote $ "Original String: " <> T.unpack srcInitial
            footnote $ "Printed String:  " <> T.unpack srcFinal

            astInitial === astFinal

        it "same symbol has the same id" $ do
            -- a a a
            let tokens =
                    locate
                        [ TSymbol "a"
                        , TSymbol "a"
                        , TSymbol "a"
                        , TEof
                        ]
            -- Next free dynamic id starts at 19
            let expectedAst =
                    locate
                        [ SSymbol 19
                        , SSymbol 19
                        , SSymbol 19
                        ]

            runAST tokens `shouldBe` Right (expectedAst, buildExpectedMap [(19, "a")])

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

            runAST tokens `shouldBe` Right (expectedAst, buildExpectedMap [])

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
                            [ Located (SSymbol (SymbolId 19)) pos
                            , Located (SNumber (NFloat 1.5)) pos
                            , Located (SNumber (NInt 2)) pos
                            ]
                        )
                        pos
                    ]
            let expectedIdMap = buildExpectedMap [(19, "add")]

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

            runAST tokens `shouldBe` Right (expectedAST, buildExpectedMap [])

        it "parses quoted expressions" $ do
            -- '(1 2)
            let tokens_sugar =
                    locate
                        [ TQuote
                        , TLeftParen
                        , TNumber "1" NTInt
                        , TNumber "2" NTInt
                        , TRightParen
                        , TEof
                        ]

            -- (quote (1 2))
            let tokens_without_sugar =
                    locate
                        [ TLeftParen
                        , TSymbol "quote"
                        , TLeftParen
                        , TNumber "1" NTInt
                        , TNumber "2" NTInt
                        , TRightParen
                        , TRightParen
                        , TEof
                        ]

            -- (quote (1 2))
            let expectedAST =
                    [ Located
                        ( SList
                            [ Located (SSymbol symQuote) pos
                            , Located (SList [Located (SNumber (NInt 1)) pos, Located (SNumber (NInt 2)) pos]) pos
                            ]
                        )
                        pos
                    ]

            runAST tokens_sugar `shouldBe` Right (expectedAST, buildExpectedMap [])
            runAST tokens_without_sugar `shouldBe` Right (expectedAST, buildExpectedMap [])

    describe "runAST error handling" $ do
        it "catches unclosed lists" $ do
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
