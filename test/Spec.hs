{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Coerce     (coerce)
import           Data.Word       (Word16)
import           Foreign.C.Types (CUShort (CUShort))
import           Numeric.Half
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary CUShort where
  arbitrary = fmap coerce (arbitrary :: Gen Word16)

instance Arbitrary Half where
  arbitrary = fmap Half arbitrary

main :: IO ()
main = hspec $ do
  describe "Half Ord instance" $ do
    it "(>=) is the opposite of (<) except for NaN" $
      property $ \x y ->
        ((x >= y) /= (x < y)) || isNaN x || isNaN (y :: Half)

    let nans = [QNaN, SNaN]

    it "returns False for NaN > NaN" $
      or [a > b | a <- nans, b <- nans] `shouldBe` False

    it "returns False for NaN < NaN" $
      or [a < b | a <- nans, b <- nans] `shouldBe` False

  describe "Round trip" $ do
    let roundTrip w = (getHalf . toHalf . fromHalf . Half $ w) == w

    it "should round trip properly" $
      property roundTrip

    it "should round trip for a NaN value" $
      roundTrip 0x7d00 `shouldBe` True
