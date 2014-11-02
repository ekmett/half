{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Numeric.Half 
  ( Half(..)
  , isZero
  , toFloat
  , toHalf
  , pattern POS_INF
  , pattern NEG_INF
  , pattern QNaN
  , pattern SNaN
  , pattern HALF_MIN
  , pattern HALF_NRM_MIN
  , pattern HALF_MAX
  , pattern HALF_EPSILON
  , pattern HALF_DIG
  , pattern HALF_MIN_10_EXP
  , pattern HALF_MAX_10_EXP
  ) where

import Data.Bits
import Data.Function (on)
import Data.Typeable
import Foreign.C.Types
import Foreign.Storable

foreign import ccall unsafe "hs_floatToHalf" toHalf :: Float -> Half
foreign import ccall unsafe "hs_halfToFloat" toFloat :: Half -> Float

{-# RULES "toHalf"  realToFrac = toHalf #-}
{-# RULES "toFloat" realToFrac = toFloat #-}

newtype Half = Half { getHalf :: CUShort } deriving (Storable, Typeable)

instance Show Half where
  showsPrec d h = showsPrec d (toFloat h)

instance Eq Half where
  (==) = (==) `on` toFloat

instance Ord Half where
  compare = compare `on` toFloat

instance Real Half where
  toRational = toRational . toFloat

instance Fractional Half where
  fromRational = toHalf . fromRational
  recip = toHalf . recip . toFloat
  a / b = toHalf $ toFloat a / toFloat b

instance RealFrac Half where
  properFraction a = case properFraction (toFloat a) of
    (b, c) -> (b, toHalf c)
  truncate = truncate . toFloat
  round = round . toFloat
  ceiling = ceiling . toFloat
  floor = floor . toFloat

instance Floating Half where
  pi = toHalf pi
  exp = toHalf . exp . toFloat
  sqrt = toHalf . sqrt . toFloat
  log = toHalf . log . toFloat
  a ** b = toHalf $ toFloat a ** toFloat b
  logBase a b = toHalf $ logBase (toFloat a) (toFloat b)
  sin = toHalf . sin . toFloat
  tan = toHalf . tan . toFloat
  cos = toHalf . cos . toFloat
  asin = toHalf . asin . toFloat
  atan = toHalf . atan . toFloat
  acos = toHalf . acos . toFloat
  sinh = toHalf . sinh . toFloat
  tanh = toHalf . tanh . toFloat
  cosh = toHalf . cosh . toFloat
  asinh = toHalf . asinh . toFloat
  atanh = toHalf . atanh . toFloat
  acosh = toHalf . acosh . toFloat

instance RealFloat Half where
  floatRadix  _ = 2
  floatDigits _ = 11
  decodeFloat = decodeFloat . toFloat
  isInfinite (Half h) = unsafeShiftR h 10 .&. 0x1f >= 32
  isIEEE _ = isIEEE (undefined :: Float)
  atan2 a b = toHalf $ atan2 (toFloat a) (toFloat b)
  isDenormalized (Half h) = unsafeShiftR h 10 .&. 0x1f == 0 && h .&. 0x3ff /= 0
  isNaN (Half h) = unsafeShiftR h 10 .&. 0x1f == 0x1f && h .&. 0x3ff /= 0
  isNegativeZero (Half h) = h == 0x8000
  floatRange _ = (16,-13)
  encodeFloat i j = toHalf $ encodeFloat i j
  exponent = exponent . toFloat
  significand = toHalf . significand . toFloat
  scaleFloat n = toHalf . scaleFloat n . toFloat

isZero :: Half -> Bool
isZero (Half h) = h .&. 0x7fff == 0

pattern POS_INF = Half 0x7c00
pattern NEG_INF = Half 0xfc00
pattern QNaN    = Half 0x7fff
pattern SNaN    = Half 0x7dff

-- | Smallest positive half
pattern HALF_MIN = 5.96046448e-08 :: Half 

-- | Smallest positive normalized half
pattern HALF_NRM_MIN = 6.10351562e-05 :: Half

-- | Largest positive half
pattern HALF_MAX = 65504.0 :: Half

-- | Smallest positive e for which half (1.0 + e) != half (1.0)
pattern HALF_EPSILON = 0.00097656 :: Half

-- | Number of base 10 digits that can be represented without change
pattern HALF_DIG = 2 

-- Minimum positive integer such that 10 raised to that power is a normalized half
pattern HALF_MIN_10_EXP = -4

-- Maximum positive integer such that 10 raised to that power is a normalized half
pattern HALF_MAX_10_EXP = 4

instance Num Half where
  a * b = toHalf (toFloat a * toFloat b)
  a - b = toHalf (toFloat a - toFloat b)
  a + b = toHalf (toFloat a + toFloat b)
  negate (Half a) = Half (xor 0x8000 a)
  abs = toHalf . abs . toFloat
  signum = toHalf . signum . toFloat
  fromInteger a = toHalf (fromInteger a)
