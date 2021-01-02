{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE PatternSynonyms #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Numeric.Half
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck (Arbitrary (..), (===), (==>), property)

instance Arbitrary Half where
  arbitrary = fmap Half arbitrary

qnan :: Half
qnan = Half 0x7fff

snan :: Half
snan = Half 0x7dff

pos_inf :: Half
pos_inf = Half 0x7c00

neg_inf :: Half
neg_inf = Half 0xfc00

nans :: [Half]
nans = [qnan, snan]

-- test QNaN, SNaN patterns

main :: IO ()
main = defaultMain $ testGroup "half"
  [ testGroup "Half Ord instance"
    [ testProperty "(>=) is the opposite of (<) except for NaN" $ \x y ->
        ((x >= y) /= (x < y)) || isNaN x || isNaN (y :: Half)

    , testProperty "returns False for NaN > NaN" $
      or [a > b | a <- nans, b <- nans] === False

    , testProperty "returns False for NaN < NaN" $
      or [a < b | a <- nans, b <- nans] === False

    ]
  , testGroup "Round trip"
    [ testProperty "should round trip properly" $ \w ->
      if isNaN w
      then property $ isNaN $ toHalf (fromHalf w) -- nans go to nans
      else toHalf (fromHalf w) === w -- everything goes to itself

    , testProperty "idempotence 1" $ \w ->
      not (isNaN w) ==> fromHalf (toHalf $ fromHalf w) === fromHalf w

    , testProperty "idempotence 2" $ \w ->
      toHalf (fromHalf $ toHalf w) === toHalf w
    ]

  , testGroup "isInfinite"
    [ testProperty "should be equivalent to \\x -> x == POS_INF || x == NEG_INF" $ \x ->
      isInfinite x === (x == pos_inf || x == neg_inf)
    , testProperty "should return True on POS_INF" $
      isInfinite pos_inf === True
    , testProperty "should return True on NEG_INF" $
      isInfinite neg_inf === True
    , testProperty "should return false on QNaN" $
      isInfinite qnan === False
    , testProperty "should return false on SNaN" $
      isInfinite snan === False
    ]

#if __GLASGOW_HASKELL__ >= 708
  , testGroup "Patterns"
    [ testProperty "QNaN" $ case qnan of
        QNaN -> True
        _    -> False
    , testProperty "SNaN" $ case snan of
        SNaN -> True
        _    -> False
    , testProperty "POS_INF" $ case pos_inf of
        POS_INF -> True
        _    -> False
    , testProperty "NEG_INF" $ case neg_inf of
        NEG_INF -> True
        _    -> False
    ]
#endif
  ]


