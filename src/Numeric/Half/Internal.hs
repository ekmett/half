{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes    #-}
#else
{-# LANGUAGE TemplateHaskell          #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms          #-}
#endif
{-# LANGUAGE Trustworthy              #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  PatternSynonyms
--
-- Half-precision floating-point values. These arise commonly in GPU work
-- and it is useful to be able to compute them and compute with them on the
-- CPU as well.
----------------------------------------------------------------------------

module Numeric.Half.Internal
  ( Half(..)
  , isZero
  , fromHalf
  , toHalf
  -- * Patterns
  -- | These are available with GHC-7.8 and later.
#if __GLASGOW_HASKELL__ >= 708
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
#endif
  -- * Pure conversions
  , pure_floatToHalf
  , pure_halfToFloat
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits
import Data.Function (on)
import Data.Int
import Data.Typeable
import Foreign.C.Types (CUShort (..))
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
#ifdef WITH_TEMPLATE_HASKELL
#endif
import Text.Read (Read (..))

import Language.Haskell.TH.Syntax (Lift (..))
#if __GLASGOW_HASKELL__ < 800
import Language.Haskell.TH
#endif

import Data.Binary (Binary (..))

#ifdef __GHCJS__
toHalf :: Float -> Half
toHalf = pure_floatToHalf

fromHalf :: Half -> Float
fromHalf = pure_halfToFloat
#else
-- | Convert a 'Float' to a 'Half' with proper rounding, while preserving NaN and dealing appropriately with infinity
foreign import ccall unsafe "hs_floatToHalf" toHalf :: Float -> Half
-- {-# RULES "toHalf"  realToFrac = toHalf #-}

-- | Convert a 'Half' to a 'Float' while preserving NaN
foreign import ccall unsafe "hs_halfToFloat" fromHalf :: Half -> Float
-- {-# RULES "fromHalf" realToFrac = fromHalf #-}
#endif

newtype
#if __GLASGOW_HASKELL__ >= 706
  {-# CTYPE "unsigned short" #-}
#endif
  Half = Half { getHalf :: CUShort } deriving (Generic, Typeable)

instance NFData Half where
#if MIN_VERSION_deepseq(1,4,0)
  rnf (Half f) = rnf f
#else
  rnf (Half f) = f `seq` ()
#endif

instance Binary Half where
  put (Half (CUShort w)) = put w
  get = fmap (Half . CUShort)  get

instance Storable Half where
  sizeOf = sizeOf . getHalf
  alignment = alignment . getHalf
  peek p = fmap Half (peek (castPtr p))
  poke p = poke (castPtr p) . getHalf

instance Show Half where
  showsPrec d h = showsPrec d (fromHalf h)

instance Read Half where
  readPrec = fmap toHalf readPrec

instance Eq Half where
  (==) = (==) `on` fromHalf

instance Ord Half where
  compare = compare `on` fromHalf
  (<) = (<) `on` fromHalf
  (<=) = (<=) `on` fromHalf
  (>) = (>) `on` fromHalf
  (>=) = (>=) `on` fromHalf

instance Real Half where
  toRational = toRational . fromHalf

instance Fractional Half where
  fromRational = toHalf . fromRational
  recip = toHalf . recip . fromHalf
  a / b = toHalf $ fromHalf a / fromHalf b

instance RealFrac Half where
  properFraction a = case properFraction (fromHalf a) of
    (b, c) -> (b, toHalf c)
  truncate = truncate . fromHalf
  round = round . fromHalf
  ceiling = ceiling . fromHalf
  floor = floor . fromHalf

instance Floating Half where
  pi = toHalf pi
  exp = toHalf . exp . fromHalf
  sqrt = toHalf . sqrt . fromHalf
  log = toHalf . log . fromHalf
  a ** b = toHalf $ fromHalf a ** fromHalf b
  logBase a b = toHalf $ logBase (fromHalf a) (fromHalf b)
  sin = toHalf . sin . fromHalf
  tan = toHalf . tan . fromHalf
  cos = toHalf . cos . fromHalf
  asin = toHalf . asin . fromHalf
  atan = toHalf . atan . fromHalf
  acos = toHalf . acos . fromHalf
  sinh = toHalf . sinh . fromHalf
  tanh = toHalf . tanh . fromHalf
  cosh = toHalf . cosh . fromHalf
  asinh = toHalf . asinh . fromHalf
  atanh = toHalf . atanh . fromHalf
  acosh = toHalf . acosh . fromHalf

instance RealFloat Half where
  floatRadix  _ = 2
  floatDigits _ = 11
  decodeFloat = ieee754_f16_decode
  isIEEE _ = isIEEE (undefined :: Float)
  atan2 a b = toHalf $ atan2 (fromHalf a) (fromHalf b)

  isInfinite (Half h) = unsafeShiftR h 10 .&. 0x1f >= 31 && h .&. 0x3ff == 0
  isDenormalized (Half h) = unsafeShiftR h 10 .&. 0x1f == 0 && h .&. 0x3ff /= 0
  isNaN (Half h) = unsafeShiftR h 10 .&. 0x1f == 0x1f && h .&. 0x3ff /= 0

  isNegativeZero (Half h) = h == 0x8000
  floatRange _ = (-13,16)
  encodeFloat i j = toHalf $ encodeFloat i j
  exponent = exponent . fromHalf
  significand = toHalf . significand . fromHalf
  scaleFloat n = toHalf . scaleFloat n . fromHalf

-- | Is this 'Half' equal to 0?
isZero :: Half -> Bool
isZero (Half h) = h .&. 0x7fff == 0

#if __GLASGOW_HASKELL__ >= 708

-- | Positive infinity
pattern POS_INF = Half 0x7c00

-- | Negative infinity
pattern NEG_INF = Half 0xfc00

-- | Quiet NaN
pattern QNaN    = Half 0x7fff

-- | Signalling NaN
pattern SNaN    = Half 0x7dff

-- | Smallest positive half
pattern HALF_MIN = Half 0x0001  -- 5.96046448e-08

-- | Smallest positive normalized half
pattern HALF_NRM_MIN = Half 0x0400  -- 6.10351562e-05

-- | Largest positive half
pattern HALF_MAX = Half 0x7bff  -- 65504.0

-- | Smallest positive e for which half (1.0 + e) != half (1.0)
pattern HALF_EPSILON = Half 0x1400  -- 0.00097656

-- | Number of base 10 digits that can be represented without change
pattern HALF_DIG = 2

-- Minimum positive integer such that 10 raised to that power is a normalized half
pattern HALF_MIN_10_EXP = -4

-- Maximum positive integer such that 10 raised to that power is a normalized half
pattern HALF_MAX_10_EXP = 4

#endif

instance Num Half where
  a * b = toHalf (fromHalf a * fromHalf b)
  a - b = toHalf (fromHalf a - fromHalf b)
  a + b = toHalf (fromHalf a + fromHalf b)
  negate (Half a) = Half (xor 0x8000 a)
  abs = toHalf . abs . fromHalf
  signum = toHalf . signum . fromHalf
  fromInteger a = toHalf (fromInteger a)

#if __GLASGOW_HASKELL__ >= 800
instance Lift Half where
  lift (Half (CUShort w)) = [| Half (CUShort w) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (Half (CUShort w)) = [|| Half (CUShort w) ||]
#endif
#else
instance Lift Half where
  lift (Half (CUShort w)) =
    appE (conE 'Half) . appE (conE 'CUShort) . litE . integerL . fromIntegral $
    w
#endif

-- Adapted from ghc/rts/StgPrimFloat.c
--
ieee754_f16_decode :: Half -> (Integer, Int)
ieee754_f16_decode (Half (CUShort i)) =
  let
      _HHIGHBIT                       = 0x0400
      _HMSBIT                         = 0x8000
      _HMINEXP                        = ((_HALF_MIN_EXP) - (_HALF_MANT_DIG) - 1)
      _HALF_MANT_DIG                  = floatDigits (undefined::Half)
      (_HALF_MIN_EXP, _HALF_MAX_EXP)  = floatRange  (undefined::Half)

      high1 = fromIntegral i
      high2 = high1 .&. (_HHIGHBIT - 1)

      exp1  = ((fromIntegral high1 `unsafeShiftR` 10) .&. 0x1F) + _HMINEXP
      exp2  = exp1 + 1

      (high3, exp3)
            = if exp1 /= _HMINEXP
                then (high2 .|. _HHIGHBIT, exp1)
                else
                      let go (!h, !e) =
                            if h .&. _HHIGHBIT /= 0
                              then go (h `unsafeShiftL` 1, e-1)
                              else (h, e)
                      in
                      go (high2, exp2)

      high4 = if fromIntegral i < (0 :: Int16)
                then -high3
                else  high3
  in
  if high1 .&. complement _HMSBIT == 0
    then (0,0)
    else (high4, exp3)

-- | Naive pure-Haskell implementation of 'toHalf'.
--
pure_floatToHalf :: Float -> Half
pure_floatToHalf = Half . pure_floatToHalf'

pure_floatToHalf' :: Float -> CUShort
pure_floatToHalf' x | isInfinite x = if x < 0 then 0xfc00 else 0x7c00
pure_floatToHalf' x | isNaN x = 0xfe00
-- for some reason, comparing with 0 and then deciding sign fails with GHC-7.8
pure_floatToHalf' x | isNegativeZero x = 0x8000
pure_floatToHalf' 0 = 0
pure_floatToHalf' x = let
  (m, n) = decodeFloat x
  -- sign bit
  s = if signum m < 0 then 0x8000 else 0
  m1 = fromIntegral $ abs m :: Int
  -- bit len of m1, here m1 /= 0
  len = 1 + snd (foldl (\(acc, res) y -> if acc .&. y == 0
                                         then (acc,       2*res)
                                         else (acc .&. y, 2*res + 1))
                       (m1, 0)
                       [ 0xffff0000, 0xff00ff00ff00, 0xf0f0f0f0
                       , 0xcccccccc, 0xaaaaaaaa]
                )
  -- scale to at least 12bit
  (len', m', n') = if len > 11 then (len, m1, n)
                   else (12, shiftL m1 (11 - len), n - (11 - len))
  e = n' + len' - 1
  in
  if e > 15 then fromIntegral (s .|. 0x7c00)
  else if e >= -14 then let t' = len' - 11
                            m'' = m' + (2 ^ (t' - 1) - 1) +
                                  (shiftR m' t' .&. 1)
                            len'' = if testBit m'' len then len' + 1 else len'
                            t'' = len'' - 11
                            e'' = n' + len'' - 1
                            res = (shiftR m'' t'' .&. 0x3ff) .|.
                                  shiftL ((e'' + 15) .&. 0x1f) 10 .|.
                                  s
                            in if e'' > 15
                               then fromIntegral (s .|. 0x7c00)
                               else fromIntegral res
  -- subnormal
  else if e >= -25 then let t = -n' + 1 -11 - 14
                            m'' = m' + (2 ^ (t - 1) - 1) +
                                  (shiftR m' t .&. 1)
                            res = shiftR m'' t .|. s
                            in if e == -15 && testBit m'' (10 + t)
                               then fromIntegral $ (shiftR m'' t .&. 0x3ff) .|.
                                                   0x400 .|. s
                               else fromIntegral res
  else fromIntegral s

-- | Naive pure-Haskell implementation of 'fromHalf'.
pure_halfToFloat :: Half -> Float
pure_halfToFloat = pure_halfToFloat' . getHalf

pure_halfToFloat' :: CUShort -> Float
pure_halfToFloat' 0xfc00 = -1/0
pure_halfToFloat' 0x7c00 =  1/0
pure_halfToFloat' 0x0000 =  0
pure_halfToFloat' 0x8000 = -0
pure_halfToFloat' x | (x .&. 0x7c00 == 0x7c00) && (x .&. 0x3ff /= 0) = 0/0
pure_halfToFloat' x = let
  s = if x .&. 0x8000 /= 0 then -1 else 1
  e = fromIntegral (shiftR x 10) .&. 0x1f :: Int
  m = x .&. 0x3ff
  (a, b) = if e > 0 then (e - 15 - 10, m .|. 0x400)
           else (-15 - 10 + 1, m)
  in encodeFloat (s * fromIntegral b) a
