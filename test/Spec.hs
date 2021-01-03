{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE PatternSynonyms #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Numeric.Half
import Numeric.Half.Internal
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Property, counterexample, (===), (==>), property, once)

import Foreign.C.Types
import Data.List (sort)

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
main = defaultMain
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

  -- With GHCJS these tests are trivially true.
  , testGroup "Native fromHalf against C version"
    [ testProperty "for full CUShort range, both version of fromHalf should return same Float" $
      once prop_from_half_list
    ]

  , testGroup "Native toHalf against C version"
    [ testProperty "for selected range of Float, both version of toHalf should return same Half" $
      once prop_to_half_list
    ]
  ]

-- test native haskell implementation of toHalf & fromHalf against with C version
prop_from_half :: CUShort -> Bool
prop_from_half i = let
  ref = fromHalf         $ Half i
  imp = pure_halfToFloat $ Half i
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
  imp = getHalf $ pure_floatToHalf i
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

prop_to_half_list :: FloatList -> Property
prop_to_half_list (FloatList l) = counterexample
    (show [ (getHalf (toHalf f), getHalf (pure_floatToHalf f), f, isNegativeZero f) | f <- take 3 l])
    $ all id $ map prop_to_half l
