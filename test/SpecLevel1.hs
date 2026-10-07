module SpecLevel1 where

import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testDict
  , testShowTypeList
  , testDefunctionalization
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
