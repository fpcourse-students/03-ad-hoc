module SpecLevel2 where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Hashable (hash)
import Data.Map qualified as Map
import Defs
import Level2
import Test.Prelude

tests :: NamedTests
tests = nameTests 2
  [ testHFoldMap
  , testTransform
  , testDefunctionalization
  , testCache
  ]

hexample :: HList '[Maybe Int, Int]
hexample = HCons (Just 42) $ HCons 1 HNil

testHFoldMap :: Test
testHFoldMap = TestList
  [ TestCase $ assertEqual "sumParticular" 43 $ sumParticular hexample
  , TestCase $ assertEqual "sumParticular, Nothing" 7 $ sumParticular (HCons Nothing $ HCons 7 HNil)
  , TestCase $ assertEqual "hfoldMap: порядок элементов" ["1", "True", "x"] $
      hfoldMap describe (HCons 1 $ HCons True $ HCons 'x' HNil)
  , TestCase $ assertEqual "hfoldMap: пустой список" "" $ hfoldMap (\case {}) HNil
  ]
  where
    describe :: HSum '[Int, Bool, Char] -> [String]
    describe = \case
      Here n -> [show n]
      There (Here b) -> [show b]
      There (There (Here c)) -> [[c]]
      There (There (There s)) -> case s of {}

-- Типы и инстансы только для тестов 2.2: решение студента про них не знает.
data Apple = Apple
data Pear = Pear

instance [String] <-- Apple where
  transform Apple = ["apple"]

instance [String] <-- Pear where
  transform Pear = ["pear"]

testTransform :: Test
testTransform = TestList
  [ TestCase $ assertEqual "sumParticular'" 43 $ sumParticular' hexample
  , TestCase $ assertEqual "sumParticular', Nothing" 7 $ sumParticular' (HCons Nothing $ HCons 7 HNil)
  , TestCase $ assertEqual "hfoldMap': порядок элементов" ["apple", "pear", "apple"] $
      hfoldMap' @[String] (HCons Apple $ HCons Pear $ HCons Apple HNil)
  , TestCase $ assertEqual "hfoldMap': пустой список" ([] :: [String]) $ hfoldMap' HNil
  ]

-- | Описание предиката: по нему строятся и предикат-данные студента, и предикат-функция.
data PredSpec = SpecEven | SpecGreater Int | SpecBoth PredSpec PredSpec
  deriving Show

instance Arbitrary PredSpec where
  arbitrary = sized go
    where
      go size
        | size <= 1 = oneof leaves
        | otherwise = oneof $ (SpecBoth <$> go (size `div` 2) <*> go (size `div` 2)) : leaves
      leaves = [pure SpecEven, SpecGreater <$> choose (-20, 20)]

toPred :: PredSpec -> Pred
toPred = \case
  SpecEven -> isEven
  SpecGreater n -> isGreater n
  SpecBoth p q -> isBoth (toPred p) (toPred q)

toFunction :: PredSpec -> Int -> Bool
toFunction = \case
  SpecEven -> even
  SpecGreater n -> (> n)
  SpecBoth p q -> \x -> toFunction p x && toFunction q x

testDefunctionalization :: Test
testDefunctionalization = TestList
  [ TestCase $ assertEqual "isEven" (evens [1 .. 10]) $ filterFO isEven [1 .. 10]
  , TestCase $ assertEqual "isGreater" (greaterThan 3 [1 .. 10]) $ filterFO (isGreater 3) [1 .. 10]
  , TestCase $ assertEqual "isBoth" (both even (> 3) [1 .. 10]) $
      filterFO (isBoth isEven (isGreater 3)) [1 .. 10]
  , TestCase $ assertEqual "isBoth вложенный" [8, 10] $
      filterFO (isBoth (isBoth isEven (isGreater 3)) (isGreater 6)) [1 .. 10]
  , TestCase $ assertBool "applyPred isEven 4" $ applyPred isEven 4
  , TestCase $ assertBool "applyPred (isGreater 3) 3" $ not $ applyPred (isGreater 3) 3
  , TestCase $ assertBool "одинаково построенные предикаты равны" $
      isBoth isEven (isGreater 3) == isBoth isEven (isGreater 3)
  , TestCase $ assertBool "предикаты с разными полями различны" $ isGreater 3 /= isGreater 4
  , TestCase $ assertBool "предикаты разных мест создания различны" $ isEven /= isGreater 0
  , propertyToTest "filterFO совпадает с filterHO" \spec (xs :: [Int]) ->
      filterFO (toPred spec) xs === filterHO (toFunction spec) xs
  ]

-- В кеше тестов лежат только значения типа Integer.
instance Eq Dynamic where
  dyn1 == dyn2 = fromDynamic dyn1 == fromDynamic @Integer dyn2

testCache :: Test
testCache = TestList
  [ TestCase $ assertEqual "storeCache, затем getCache" (Just 5) $
      evalCached (storeCache key (5 :: Integer) *> getCache @Integer key)
  , TestCase $ assertEqual "getCache: ключа нет" Nothing $
      evalCached (getCache @Integer key)
  , TestCase $ assertEqual "getCache: под ключом значение другого типа" Nothing $
      evalCached (storeCache (newKey 'x') True *> getCache @Integer key)
  , TestCase $ assertEqual "повторный вызов с теми же аргументами" (4, Map.fromList [(hash (singleton 3), toDyn @Integer 4)]) $
      runCached $ let f = cached sumNFibs in f (singleton 3) *> f (singleton 3)
  , TestCase $ assertEqual "вызовы с разными аргументами"
      (11, Map.fromList [(hash (singleton 3), toDyn @Integer 4), (hash (singleton 4), toDyn @Integer 7)]) $
      runCached $ let f = cached sumNFibs in (+) <$> f (singleton 3) <*> f (singleton 4)
  , TestCase $ assertEqual "значение из кеша важнее вычисления" 100 $
      evalCached $ storeCache (newKey (singleton 3)) (100 :: Integer) *> cached sumNFibs (singleton 3)
  , TestCase $ assertEqual "testCached" 11 $ testCached 3 4
  ]
  where
    key :: Key Integer
    key = newKey 'x'

    singleton :: Integer -> HList '[Integer]
    singleton n = HCons n HNil
