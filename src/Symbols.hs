{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Symbols (
    SymbolId (..),
    symQuote,
    symDefine,
    symIf,
    symLambda,
    symLet,
    symPlus,
    symMinus,
    symMult,
    symDiv,
    symEq,
    symGt,
    symLt,
    symNot,
    symCons,
    symCar,
    symCdr,
    symList,
    symIsNull,
    symDisplay,
    specialSymbols,
    specialSymbolsNumber,
) where

import Data.Hashable (Hashable (..))
import Data.Text

newtype SymbolId = SymbolId {getId :: Int} deriving (Show, Eq, Ord, Num)

instance Hashable SymbolId where
    hash (SymbolId sId) = sId
    hashWithSalt salt (SymbolId sId) = hashWithSalt salt sId

-- Hardcoded constants for rapid querying
symQuote, symDefine, symIf, symLambda, symLet :: SymbolId
symPlus, symMinus, symMult, symDiv, symEq, symGt, symLt :: SymbolId
symNot, symCons, symCar, symCdr, symList, symIsNull, symDisplay :: SymbolId
symQuote = SymbolId 0
symDefine = SymbolId 1
symIf = SymbolId 2
symLambda = SymbolId 3
symLet = SymbolId 4

symPlus = SymbolId 5
symMinus = SymbolId 6
symMult = SymbolId 7
symDiv = SymbolId 8
symEq = SymbolId 9
symGt = SymbolId 10
symLt = SymbolId 11
symNot = SymbolId 12
symCons = SymbolId 13
symCar = SymbolId 14
symCdr = SymbolId 15
symList = SymbolId 16
symIsNull = SymbolId 17
symDisplay = SymbolId 18

specialSymbols :: [(Text, SymbolId)]
specialSymbols =
    [ ("quote", symQuote)
    , ("define", symDefine)
    , ("if", symIf)
    , ("lambda", symLambda)
    , ("let", symLet)
    , ("+", symPlus)
    , ("-", symMinus)
    , ("*", symMult)
    , ("/", symDiv)
    , ("=", symEq)
    , (">", symGt)
    , ("<", symLt)
    , ("not", symNot)
    , ("cons", symCons)
    , ("car", symCar)
    , ("cdr", symCdr)
    , ("list", symList)
    , ("null?", symIsNull)
    , ("display", symDisplay)
    ]

specialSymbolsNumber :: Int
specialSymbolsNumber = 18
