module Numbers (Number (..), compareNums) where

data Number = NFloat Double | NInt Integer deriving (Show, Eq)

instance Num Number where
    (NInt a) + (NInt b) = NInt (a + b)
    (NFloat a) + (NFloat b) = NFloat (a + b)
    (NInt a) + (NFloat b) = NFloat (fromIntegral a + b)
    (NFloat a) + (NInt b) = NFloat (a + fromIntegral b)

    (NInt a) - (NInt b) = NInt (a - b)
    (NFloat a) - (NFloat b) = NFloat (a - b)
    (NInt a) - (NFloat b) = NFloat (fromIntegral a - b)
    (NFloat a) - (NInt b) = NFloat (a - fromIntegral b)

    (NInt a) * (NInt b) = NInt (a * b)
    (NFloat a) * (NFloat b) = NFloat (a * b)
    (NInt a) * (NFloat b) = NFloat (fromIntegral a * b)
    (NFloat a) * (NInt b) = NFloat (a * fromIntegral b)

    negate (NInt a) = NInt (negate a)
    negate (NFloat a) = NFloat (negate a)

    fromInteger n = NInt n

    abs (NInt a) = NInt (abs a)
    abs (NFloat a) = NFloat (abs a)
    signum (NInt a) = NInt (signum a)
    signum (NFloat a) = NFloat (signum a)

instance Fractional Number where
    -- Division always converts integers to floats to prevent truncation errors
    (NFloat a) / (NFloat b) = NFloat (a / b)
    (NInt a) / (NInt b) = NFloat (fromIntegral a / fromIntegral b)
    (NInt a) / (NFloat b) = NFloat (fromIntegral a / b)
    (NFloat a) / (NInt b) = NFloat (a / fromIntegral b)

    fromRational r = NFloat (fromRational r)

compareNums :: Number -> Number -> Ordering
compareNums (NInt a) (NInt b)     = compare a b
compareNums (NFloat a) (NFloat b) = compare a b
compareNums (NInt a) (NFloat b)   = compare (fromIntegral a) b
compareNums (NFloat a) (NInt b)   = compare a (fromIntegral b)
