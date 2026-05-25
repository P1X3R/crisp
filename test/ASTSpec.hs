module ASTSpec (spec) where

import AST (Number (..), SExpr (..), SymbolId (..), runAST)
import qualified Data.Map as M
import Hedgehog
import Lexer (NumberType (..), Position (..), Token (..), TokenData (..), runTokenizer)
import ProgramHelpers (genExpr, showSExpr)
import Test.Hspec
import Test.Hspec.Hedgehog

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
