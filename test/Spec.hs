{-# LANGUAGE LambdaCase #-}

import Data.List (intercalate, uncons)
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
    first <- Gen.choice [Gen.alpha, Gen.element specialSymbols]
    rest <-
        Gen.string (Range.linear (minSymbolLen - 1) (maxSymbolLen - 1)) $
            Gen.choice [Gen.alphaNum, Gen.element specialSymbols]
    pure $ first : rest

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
                    mapM_ (\(pos1, pos2) -> ((pColumn pos2) > (pColumn pos1) || (pLine pos2) > (pLine pos1)) === True) pairs