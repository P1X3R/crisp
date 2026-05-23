module ProgramGen (
    genProgram,
) where

import Data.List (intercalate)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric (showFFloat)

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
    return $ "\"" <> content <> "\""

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

genCommentedExpr :: Gen String
genCommentedExpr = do
    comment <- Gen.string (Range.linear 0 maxStrLen) Gen.alphaNum
    expr <- genExpr
    return $ "; " ++ comment ++ "\n" ++ expr

genProgram :: Gen String
genProgram = do
    lines' <- Gen.list (Range.linear minProgramLen maxProgramLen) genCommentedExpr
    return $ intercalate "\n" lines'
