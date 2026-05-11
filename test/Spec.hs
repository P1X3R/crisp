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
