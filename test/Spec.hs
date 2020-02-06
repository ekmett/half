{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Numeric.Half
import           Test.Hspec
import           Test.QuickCheck

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
    it "should round trip properly" $
      property $ \w -> if isNaN w
        then property $ isNaN $ toHalf (fromHalf w) -- nans go to nans
        else toHalf (fromHalf w) === w -- everything goes to itself
    it "idempotence 1" $ do
      property $ \w -> not (isNaN w) ==> fromHalf (toHalf $ fromHalf w) === fromHalf w
    it "idempotence 2" $ do
      property $ \w -> toHalf (fromHalf $ toHalf w) === toHalf w

  describe "isInfinite" $ do
    it "should be equivalent to \\x -> x == POS_INF || x == NEG_INF" $
      property $ \x -> isInfinite x === (x == POS_INF || x == NEG_INF)
    it "should return True on POS_INF" $
      isInfinite POS_INF `shouldBe` True
    it "should return True on NEG_INF" $
      isInfinite NEG_INF `shouldBe` True
    it "should return false on QNaN" $
      isInfinite QNaN `shouldBe` False
    it "should return false on SNaN" $
      isInfinite SNaN `shouldBe` False
