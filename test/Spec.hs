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

#if __GLASGOW_HASKELL__ >= 708
  , testGroup "Patterns"
    [ testProperty "QNaN" $ case qnan of
        QNaN -> True
        _    -> False
    , testProperty "SNaN" $ case snan of
        SNaN -> True
        _    -> False
    ]
#endif
  ]
