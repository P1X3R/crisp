{-# LANGUAGE LambdaCase #-}

import Data.List (intercalate, uncons)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer (LangError (..), LexerDetail (..), Position (..), Token (..), TokenData (..), runTokenizer)
import Numeric (showFFloat)
import Test.Hspec
import Test.Hspec.Hedgehog

minInt :: Int
minInt = -100
maxInt :: Int
maxInt = 100

minFloat :: Float
minFloat = -1.0
maxFloat :: Float
maxFloat = 1.0

minStrLen :: Int
minStrLen = 0
maxStrLen :: Int
maxStrLen = 100

minSymbolLen :: Int
minSymbolLen = 1
maxSymbolLen :: Int
maxSymbolLen = 20

minExprLen :: Int
minExprLen = 0
maxExprLen :: Int
maxExprLen = 10

minProgramLen :: Int
minProgramLen = 1
maxProgramLen :: Int
maxProgramLen = 12

genNumber :: Gen String
genNumber =
    let randInt = show <$> Gen.int (Range.linear minInt maxInt)
        randFloat = (\f -> showFFloat Nothing f "") <$> Gen.float (Range.linearFrac minFloat maxFloat)
     in Gen.choice [randInt, randFloat]

genBool :: Gen String
genBool = Gen.element ["#t", "#f"]

genString :: Gen String
genString = do
    content <- Gen.string (Range.linear minStrLen maxStrLen) Gen.alphaNum
    pure $ "\"" <> content <> "\""

genSymbol :: Gen String
genSymbol = do
    let specialSymbols = "+-*/><=!?_"
    let nonDigit = Gen.choice [Gen.alpha, Gen.element specialSymbols]
    let anySymbol = Gen.choice [Gen.alphaNum, Gen.element specialSymbols]

    len <- Gen.int (Range.linear minSymbolLen maxSymbolLen)

    -- The first two characters are not digits, the rest can be digits
    let genList' = replicate (min 2 len) nonDigit ++ replicate (max 0 (len - 2)) anySymbol

    sequence genList'

genAtom :: Gen String
genAtom = Gen.choice [genNumber, genBool, genString, genSymbol]

genList :: Gen String
genList = (\s -> "(" <> unwords s <> ")") <$> Gen.list (Range.linear minExprLen maxExprLen) genExpr

genQuote :: Gen String
genQuote = ("'" <>) <$> genExpr

genExpr :: Gen String
genExpr = Gen.recursive Gen.choice [genAtom] [genList, genQuote]

genProgram :: Gen String
genProgram = do
    exprs <- Gen.list (Range.linear minProgramLen maxProgramLen) genExpr
    commentsContent <- Gen.list (Range.linear minProgramLen maxProgramLen) $ Gen.string (Range.linear 0 maxStrLen) Gen.alphaNum
    let comments = map ("; " ++) commentsContent
    let program = zipWith (\s1 s2 -> s1 ++ "\n" ++ s2) comments exprs
    pure $ intercalate "\n" program

advanceUntilMatch :: String -> Position -> Position -> String
advanceUntilMatch "" _ _ = ""
advanceUntilMatch code@(c : cs) pos posAcc
    | pos == posAcc = code
    | otherwise = advanceUntilMatch cs pos (movePosition posAcc c)
  where
    movePosition (Position l _) '\n' = Position (l + 1) 1
    movePosition (Position l col) _ = Position l (col + 1)

main :: IO ()
main = hspec $ do
    describe "runTokenizer" $ do
        it "empty program is just eof" $ do
            runTokenizer "" `shouldBe` Right [Token TEof (Position 1 1)]

        it "a minus sign with no digit is a symbol" $ do
            runTokenizer "-" `shouldBe` Right [Token (TSymbol $ T.pack "-") (Position 1 1), Token TEof (Position 1 2)]
            runTokenizer "-abc"
                `shouldBe` Right
                    [Token (TSymbol $ T.pack "-abc") (Position 1 1), Token TEof (Position 1 5)]

        it "property: valid syntax never fails" $ hedgehog $ do
            input <- forAll $ genProgram

            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> case last t of
                    Token TEof _ -> success
                    _ -> failure

        it "property: tokens' position always strictly advance" $ hedgehog $ do
            input <- forAll $ genProgram

            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> do
                    annotateShow t
                    let positions = map tPosition t
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

                    rebuiltLists <- evalEither $ mapM (\(Token _ pos) -> runTokenizer $ advanceUntilPos pos) t
                    rebuiltTokens <- evalMaybe $ mapM getFirst rebuiltLists

                    -- only check for token data because position data in `rebuiltTokens` is always off
                    map tData rebuiltTokens === map tData t

    describe "runTokenizer error handling" $ do
        it "catches unclosed strings (hits 0% if condition)" $ do
            runTokenizer "\"hello" `shouldBe` Left (LELexerError LDUnclosedString (Position 1 1))

        it "catches multiple decimals in a number (hits unevaluated guard)" $ do
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
