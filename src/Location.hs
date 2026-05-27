module Location (
    Position (..),
    Located (..),
) where

data Position = Position
    { pLine :: Int
    , pColumn :: Int
    }
    deriving (Show, Eq, Ord)

data Located a = Located { unLocated :: a, lPosition :: Position } deriving (Show, Eq)
