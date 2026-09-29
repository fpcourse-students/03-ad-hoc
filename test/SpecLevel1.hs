module SpecLevel1 where

import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testDict
  , testShowTypeList
  , testReify
  , testProof
  ]

testDict :: Test
testDict = TestList
  [ TestCase $ assertEqual "foldDict sumDict" 10 $ foldDict sumDict [1, 2, 3, 4]
  , TestCase $ assertEqual "foldDict sumDict []" 0 $ foldDict sumDict []
  , TestCase $ assertEqual "foldDict productDict" 120 $ foldDict productDict [1 .. 5]
  , TestCase $ assertEqual "foldDict productDict []" 1 $ foldDict productDict []
  , TestCase $ assertEqual "foldDict для своего словаря" "abc" $
      foldDict (MonoidDict "" (++)) ["a", "b", "c"]
  , TestCase $ assertEqual "pairDict" (9, 48) $
      foldDict (pairDict sumDict productDict) [(1, 2), (3, 4), (5, 6)]
  , TestCase $ assertEqual "pairDict []" (0, 1) $
      foldDict (pairDict sumDict productDict) []
  , TestCase $ assertEqual "pairDict вложенный" ((6, 6), "xyz") $
      foldDict (pairDict (pairDict sumDict productDict) (MonoidDict "" (++)))
        [((1, 1), "x"), ((2, 2), "y"), ((3, 3), "z")]
  , propertyToTest "foldDict sumDict = sum" \(xs :: [Int]) ->
      foldDict sumDict xs === sum xs
  , propertyToTest "foldDict productDict = product" \(xs :: [Int]) ->
      foldDict productDict xs === product xs
  ]

testShowTypeList :: Test
testShowTypeList = TestList
  [ TestCase $ assertEqual "пустой список" "[]" $ showTypeList @'[]
  , TestCase $ assertEqual "один тип" "[Bool]" $ showTypeList @'[Bool]
  , TestCase $ assertEqual "два типа" "[Int,Double]" $ showTypeList @'[Int, Double]
  , TestCase $ assertEqual "тип с аргументом" "[Maybe,Char,Int]" $
      showTypeList @'[Maybe Int, Char, Int]
  ]

testReify :: Test
testReify = TestList
  [ TestCase $ assertEqual "reify 0" 0 $ reify 0 \(_ :: Proxy n) -> natVal @n
  , TestCase $ assertEqual "reify 3" 3 $ reify 3 \(_ :: Proxy n) -> natVal @n
  , TestCase $ assertEqual "reify (-5)" 0 $ reify (-5) \(_ :: Proxy n) -> natVal @n
  , TestCase $ assertEqual "reify: результат любого типа" "2" $
      reify 2 \(_ :: Proxy n) -> show (natVal @n)
  , TestCase $ assertEqual "wonderId (-3)" 0 $ wonderId (-3)
  , propertyToTest "wonderId" \(NonNegative n) ->
      n <= 1000 ==> wonderId n === n
  ]

testProof :: Test
testProof = TestList
  [ TestCase $ assertEqual "слева направо, Left" "42" $ leftToRight (show, yesNo) (Left 42)
  , TestCase $ assertEqual "слева направо, Right" "yes" $ leftToRight (show, yesNo) (Right True)
  , TestCase $ assertEqual "справа налево, первая компонента" "1" $
      fst (rightToLeft (either show yesNo)) 1
  , TestCase $ assertEqual "справа налево, вторая компонента" "no" $
      snd (rightToLeft (either show yesNo)) False
  ]
  where
    leftToRight :: (Int -> String, Bool -> String) -> Either Int Bool -> String
    rightToLeft :: (Either Int Bool -> String) -> (Int -> String, Bool -> String)
    (leftToRight, rightToLeft) = a8Like
    yesNo b = if b then "yes" else "no"
