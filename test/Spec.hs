{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Numeric.Half
import           Test.Hspec
import           Test.QuickCheck

import Foreign.C.Types
import Data.List (sort)

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

  describe "Native fromHalf against C version" $ do
    it "for full CUShort range, both version of fromHalf should return same Float" $
      property (withMaxSuccess 1 prop_from_half_list)

  describe "Native toHalf against C version" $ do
    it "for selected range of Float, both version of toHalf should return same Half" $
      property (withMaxSuccess 1 prop_to_half_list)


-- test native haskell implementation of toHalf & fromHalf against with C version
prop_from_half :: CUShort -> Bool
prop_from_half i = let
  ref = fromHalf $ Half i
  imp = hs_halfToFloat' i
  in (isNaN ref && isNaN imp) || (ref == imp)

newtype U16List = U16List [CUShort] deriving (Eq, Ord, Show)

instance Arbitrary U16List where
  arbitrary = return (U16List [0 .. 65535])
  shrink (U16List (_ : [])) = []
  shrink (U16List x) = let p = length x `div` 2
                       in [U16List $ take p x, U16List $ drop p x]

prop_from_half_list :: U16List -> Bool
prop_from_half_list (U16List l) = all id $ map prop_from_half l

prop_to_half :: Float -> Bool
prop_to_half i = let
  ref = getHalf $ toHalf i
  imp = hs_floatToHalf' i
  in ref == imp

-- cover all range of Half(not Float)
list1 :: [Float]
list1 = let
  r1 = filter (not . isNaN) $ map (fromHalf . Half) [0 .. 65535]
  r2 = sort $ filter (not . isInfinite) $ filter (>= 0) r1
  r3 = r2 ++ [last r2 + 2 ** 11]
  r4 = zipWith (\a b -> let d = (b - a) / 4
                        in [a, a + d, a + d * 2, a + d * 3])
               r3 (tail r3)
  r5 = concat r4 ++ [last r3]
  in r5

list2 :: [Float]
list2 = map negate list1

list3 :: [Float]
list3 = [1/0, -1/0, 0, -0, 0/0]


newtype FloatList = FloatList [Float] deriving (Eq, Ord, Show)

instance Arbitrary FloatList where
  arbitrary = return (FloatList $ list1 ++ list2 ++ list3)
  shrink (FloatList (_ : [])) = []
  shrink (FloatList x) = let p = length x `div` 2
                         in [FloatList $ take p x, FloatList $ drop p x]

prop_to_half_list :: FloatList -> Bool
prop_to_half_list (FloatList l) = all id $ map prop_to_half l
