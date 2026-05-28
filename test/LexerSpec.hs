{-# LANGUAGE LambdaCase #-}

module LexerSpec (spec) where

import Data.List (uncons)
import qualified Data.Text as T
import Hedgehog
import Lexer (Token (..), runTokenizer)
import LanguageError (LangError (..), LexerDetail (..))
import ProgramHelpers (genProgram)
import Test.Hspec
import Test.Hspec.Hedgehog
import Location (Located(..), Position (..))

advanceUntilMatch :: String -> Position -> Position -> String
advanceUntilMatch "" _ _ = ""
advanceUntilMatch code@(c : cs) pos posAcc
    | pos == posAcc = code
    | otherwise = advanceUntilMatch cs pos (movePosition posAcc c)
  where
    movePosition (Position l _) '\n' = Position (l + 1) 1
    movePosition (Position l col) _ = Position l (col + 1)

spec :: Spec
spec = do
    describe "runTokenizer" $ do
        it "empty program is just eof" $ do
            runTokenizer "" `shouldBe` Right [Located TEof (Position 1 1)]

        it "a minus sign with no digit is a symbol" $ do
            runTokenizer "-" `shouldBe` Right [Located (TSymbol $ T.pack "-") (Position 1 1), Located TEof (Position 1 2)]
            runTokenizer "-abc"
                `shouldBe` Right
                    [Located (TSymbol $ T.pack "-abc") (Position 1 1), Located TEof (Position 1 5)]

        it "property: valid syntax never fails" $ hedgehog $ do
            input <- forAll $ genProgram

            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> case last t of
                    Located TEof _ -> success
                    _ -> failure

        it "property: tokens' position always strictly advance" $ hedgehog $ do
            input <- forAll $ genProgram

            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> do
                    annotateShow t
                    let positions = map lPosition t
                    let pairs = zip positions (drop 1 positions)
                    not (null pairs) === True
                    mapM_ (\(pos1, pos2) -> (pos1 < pos2) === True) pairs

        it "property: lexer can resume tokenization from any valid atom token's starting position" $ hedgehog $ do
            withParen <- forAll $ genProgram

            -- filter out parentheses from input becuase otherwise the lexer would encounter an unclosed list
            let input = filter (\c -> notElem c "()") withParen
            annotateShow input

            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> do
                    let getFirst l = do
                            (frst, _) <- uncons l
                            Just frst
                    let advanceUntilPos pos = advanceUntilMatch input pos (Position 1 1)

                    rebuiltLists <- evalEither $ mapM (\(Located _ pos) -> runTokenizer $ advanceUntilPos pos) t
                    rebuiltTokens <- evalMaybe $ mapM getFirst rebuiltLists

                    -- only check for token data because position data in `rebuiltTokens` is always off
                    map unLocated rebuiltTokens === map unLocated t

    describe "runTokenizer error handling" $ do
        it "catches unclosed strings" $ do
            runTokenizer "\"hello" `shouldBe` Left (LELexerError LDUnclosedString (Position 1 1))

        it "catches multiple decimals in a number" $ do
            runTokenizer "12.34.56" `shouldBe` Left (LELexerError LDMultipleDotInNumber (Position 1 6))

        it "catches numbers with leading letters" $ do
            runTokenizer "1a" `shouldBe` Left (LELexerError LDInvalidNumber (Position 1 2))
            runTokenizer "1-" `shouldBe` Left (LELexerError LDInvalidNumber (Position 1 2))
            runTokenizer "1-1" `shouldBe` Left (LELexerError LDInvalidNumber (Position 1 2))

        it "catches an extra closing parenthesis" $ do
            runTokenizer "())" `shouldBe` Left (LELexerError LDExtraParenthesis (Position 1 3))

        it "catches unclosed lists at EOF" $ do
            runTokenizer "(abc" `shouldBe` Left (LELexerError LDUnclosedList (Position 1 1))

        it "catches malformed booleans" $ do
            runTokenizer "#x" `shouldBe` Left (LELexerError LDInvalidBool (Position 1 2))
            runTokenizer "#tabcd" `shouldBe` Left (LELexerError LDInvalidBool (Position 1 3))

        it "catches invalid symbols (hits final fallback parser)" $ do
            runTokenizer "@" `shouldBe` Left (LELexerError LDInvalidSymbolChar (Position 1 1))
