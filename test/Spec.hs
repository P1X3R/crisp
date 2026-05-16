import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer (ParserState (..), Position (..), Token (..), TokenData (..), runTokenizer)
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
minStrLen = 10
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

genNumber :: Gen String
genNumber =
    let randInt = fmap show $ Gen.int (Range.linear minInt maxInt)
        randFloat = fmap show $ Gen.float (Range.linearFrac minFloat maxFloat)
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

genExpr :: Gen String
genExpr =
    Gen.recursive
        Gen.choice
        [ genAtom
        ]
        [ fmap (\s -> "(" <> unwords s <> ")") (Gen.list (Range.linear minExprLen maxExprLen) genExpr)
        , fmap ("'" <>) genExpr
        ]

main :: IO ()
main = hspec $ do
    describe "runTokenizer" $ do
        it "property: empty program is just eof" $ do
            runTokenizer "" `shouldBe` Right [Token TEof (Position 1 1)]

        it "property: tokens' position always strictly advance" $ hedgehog $ do
            input <- forAll $ genExpr

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