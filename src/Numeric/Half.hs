{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms          #-}
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

module Numeric.Half
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
  ) where

import Numeric.Half.Internal
