module SpecBlock1 where

import Block1
import Data.Dynamic
import Data.Hashable
import Data.Map qualified as Map
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testShowTypeList
  , testWonderId
  , testHFoldMap
  , testCache
  ]

testShowTypeList :: Test
testShowTypeList = TestList
  [ TestCase $ assertEqual "empty list" "[]" $ showTypeList @'[]
  , TestCase $ assertEqual "non empty" "[Int,Double]" $ showTypeList @'[Int, Double]
  ]

testWonderId :: Test
testWonderId = propertyToTest "wonderId" \(NonNegative n) ->
  n == wonderId n

testHFoldMap :: Test
testHFoldMap = TestList
  [ TestCase $ assertEqual "sumParticular" 43 $ sumParticular hexample
  , TestCase $ assertEqual "sumParticular'" 43 $ sumParticular' hexample
  ]
  where
    hexample :: HList '[Maybe Int, Int]
    hexample = HCons (Just 42) $ HCons 1 HNil

testCache :: Test
testCache = TestList
  [ TestCase $
    let res = 4 :: Integer in
    let cache = Map.fromList [(hash (singleton 3), toDyn res)] in
    assertEqual "Overlapping calls" (res, cache) $
      runCached $ let f = cached sumNFibs in f (singleton 3) *> f (singleton 3)
  , TestCase $
    let res1 = 4 :: Integer in
    let res2 = 7 :: Integer in
    let cache = Map.fromList
          [ (hash (singleton 3), toDyn res1)
          , (hash (singleton 4), toDyn res2) ] in
    assertEqual "Overlapping calls" (res1 + res2, cache) $
      runCached $ let f = cached sumNFibs in (+) <$> f (singleton 3) <*> f (singleton 4)
  ]
  where
    singleton :: Integer -> HList '[Integer]
    singleton n = HCons n HNil

instance Eq Dynamic where
  dyn1 == dyn2 = fromDynamic dyn1 == fromDynamic @Integer dyn2
