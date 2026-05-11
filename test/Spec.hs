import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer (ParserState (..), Position (..), Token (..), TokenData (..), runTokenizer)
import Test.Hspec
import Test.Hspec.Hedgehog

main :: IO ()
main = hspec $ do
    describe "runTokenizer" $ do
        it "property: empty program is just eof" $ do
            runTokenizer "" `shouldBe` Right [Token TEof (Position 1 1)]

        it "property: tokens' position always strictly advance" $ hedgehog $ do
            -- Generate 1 to 100 words, each 1 to 10 characters long
            xsList <- forAll $ Gen.list (Range.linear 1 100) $ Gen.string (Range.linear 1 10) Gen.alpha

            let input = unwords xsList
            case runTokenizer input of
                Left err -> do
                    annotateShow err
                    failure
                Right t -> do
                    annotateShow t
                    let positions = map (pColumn . tPosition) t
                    let pairs = zip positions (drop 1 positions)
                    not (null pairs) === True
                    mapM_ (\(pos1, pos2) -> (pos1 < pos2) === True) pairs