{-# LANGUAGE OverloadedStrings #-}

module ProgramHelpers (
    genProgram,
    genExpr,
    showSExpr,
) where

import AST (SExpr (..))
import Numbers (Number (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Location (Located (..))
import Symbols (SymbolId, symQuote)

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

genNumber :: Gen T.Text
genNumber =
    let randInt = T.pack . show <$> Gen.int (Range.linear minInt maxInt)
        randFloat =
            (\f -> TL.toStrict . B.toLazyText $ B.formatRealFloat B.Fixed Nothing f)
                <$> Gen.float (Range.linearFrac minFloat maxFloat)
     in Gen.choice [randInt, randFloat]

genBool :: Gen T.Text
genBool = Gen.element ["#t", "#f"]

genString :: Gen T.Text
genString = do
    content <- Gen.text (Range.linear minStrLen maxStrLen) Gen.alphaNum
    return $ "\"" <> content <> "\""

genSymbol :: Gen T.Text
genSymbol = Gen.filter (/= "quote") $ do
    let specialSymbols = ['+', '-', '*', '/', '>', '<', '=', '!', '?', '_']
    let nonDigit = Gen.choice [Gen.alpha, Gen.element specialSymbols]
    let anySymbol = Gen.choice [Gen.alphaNum, Gen.element specialSymbols]

    len <- Gen.int (Range.linear minSymbolLen maxSymbolLen)

    -- Generate a list of Char generators, keeping the first two non-digits
    let genList' = replicate (min 2 len) nonDigit ++ replicate (max 0 (len - 2)) anySymbol

    T.pack <$> sequence genList'

genAtom :: Gen T.Text
genAtom = Gen.choice [genNumber, genBool, genString, genSymbol]

genList :: Gen T.Text
genList = (\s -> "(" <> T.unwords s <> ")") <$> Gen.list (Range.linear minExprLen maxExprLen) genExpr

genQuote :: Gen T.Text
genQuote = ("'" <>) <$> genExpr

genExpr :: Gen T.Text
genExpr = Gen.recursive Gen.choice [genAtom] [genList, genQuote]

genCommentedExpr :: Gen T.Text
genCommentedExpr = do
    comment <- Gen.text (Range.linear 0 maxStrLen) Gen.alphaNum
    expr <- genExpr
    return $ "; " <> comment <> "\n" <> expr

genProgram :: Gen T.Text
genProgram = do
    lines' <- Gen.list (Range.linear minProgramLen maxProgramLen) genCommentedExpr
    return $ T.intercalate "\n" lines'

showSExpr :: SExpr -> HM.HashMap SymbolId T.Text -> Maybe T.Text
showSExpr (SNumber (NFloat num)) _ = Just . TL.toStrict . B.toLazyText $ B.formatRealFloat B.Fixed Nothing num
showSExpr (SNumber (NInt num)) _ = Just . T.pack $ show num
showSExpr (SBool bool) _ = Just $ case bool of
    True -> "#t"
    False -> "#f"
showSExpr (SStr content) _ = Just $ "\"" <> content <> "\""
showSExpr (SSymbol sId) idMap = HM.lookup sId idMap
showSExpr (SList [Located (SSymbol sId) _, Located expr _]) idMap
    | sId == symQuote = do
        quoted <- showSExpr expr idMap
        Just $ "'" <> quoted
showSExpr (SList elements) idMap = do
    content <- mapM (\(Located e _) -> showSExpr e idMap) elements
    Just $ "(" <> T.unwords content <> ")"
