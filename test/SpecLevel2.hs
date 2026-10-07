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
