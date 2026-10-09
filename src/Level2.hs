-- | Домашка 3. Специальный (ad-hoc) полиморфизм: уровень 2.

-- Ограничение All ((<--) m) as в 2.2 вычисляется семейством типов.
{-# LANGUAGE UndecidableInstances #-}
module Level2 where

import Control.Monad (join)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Dynamic (Dynamic)
import Data.Dynamic qualified as Dynamic
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (getSum, Sum(..))
import Data.Typeable (Typeable)
import Defs
import MetaUtils (todo)


-- 2.1. Свёртка гетерогенного списка
--
-- HSum tys — значение ровно одного из типов списка tys: Here x — первого, There s —
-- какого-то из остальных. Это сумма типов, как HList — их произведение.
--
-- hfoldMap сворачивает гетерогенный список в моноид. Обработчик у неё один на все элементы:
-- он получает элемент вместе с его позицией в списке, то есть значение HSum tys.
-- Результаты соединяются слева направо; от пустого списка — mempty.
--
-- sumParticular складывает числа списка; Nothing считается нулём. В обработчике GHC
-- попросит ветку и для There (There s), хотя значений такого вида не бывает: у s тип
-- HSum '[]. Убедить компилятор, что варианты перебраны, можно пустым case: case s of {}.

data HSum (tys :: [Type]) where
  Here :: ty -> HSum (ty ': tys)
  There :: HSum tys -> HSum (ty ': tys)

hfoldMap :: Monoid m => (HSum tys -> m) -> HList tys -> m
hfoldMap f = \case
  HNil -> mempty
  HCons el xs -> f (Here el) <> (hfoldMap (f . There) xs)

sumParticular :: HList '[Maybe Int, Int] -> Int
sumParticular = getSum . (hfoldMap help) where
  help :: HSum '[Maybe Int, Int] -> Sum Int
  help (Here Nothing) = Sum 0
  help (Here (Just x)) = Sum x
  help (There (Here y)) = Sum y
  help (There (There s)) = case s of {}


-- 2.2. Обработчики выбирает компилятор
--
-- Передать одну функцию от суммы — всё равно что передать по функции на каждый тип списка.
-- Пусть этот набор функций строит компилятор. Класс to <-- from — функция transform
-- из from в to, выбранная по обоим типам. Ограничение All ((<--) m) tys требует такую
-- функцию в m для каждого типа списка.
--
-- Реализуйте hfoldMap' и sumParticular' (то же, что sumParticular) и объявите инстансы
-- класса, которые для этого нужны.

class to <-- from where
  transform :: from -> to

instance Sum Int <-- Maybe Int where
  transform Nothing = Sum 0
  transform (Just n) = Sum n

instance Sum Int <-- Int where
  transform = Sum

hfoldMap' :: forall m tys . (Monoid m, All ((<--) m) tys) => HList tys -> m
hfoldMap' = \case
  HNil -> mempty
  HCons el xs -> transform el <> hfoldMap' xs

sumParticular' :: HList '[Maybe Int, Int] -> Int
sumParticular' = getSum . hfoldMap'


-- 2.3. Кеширующий декоратор
--
-- cached превращает функцию от произвольного числа аргументов (они собраны в HList)
-- в функцию, которая запоминает результаты. Кеш один на все функции и все типы
-- результатов, поэтому значения лежат в нём динамически типизированными (Data.Dynamic),
-- а ключом служит хеш списка аргументов. Совпадения хешей у разных аргументов не разбираем.
--
--   * newKey строит ключ по хешируемому значению; тип результата в ключе — фантомный;
--   * runCached запускает вычисление с пустым кешем и возвращает результат вместе
--     с итоговым кешем, evalCached — только результат;
--   * getCache возвращает значение по ключу; Nothing — если ключа нет или под ним лежит
--     значение другого типа;
--   * storeCache кладёт значение по ключу;
--   * cached f args: если результат для args в кеше есть, он возвращается, а f не
--     вызывается; иначе результат вычисляется, кладётся в кеш и возвращается.
--
-- Тесты смотрят и на итоговый кеш: в нём ровно по одной записи на каждый набор аргументов,
-- ключ записи — hash списка аргументов.

-- | Типизированный ключ динамического гетерогенного хранилища.
newtype Key ty = Key { getKeyHash :: Int }
  deriving newtype (Eq, Ord)

-- | Вычисление с изменяемым кешем.
type Cached a = State (Map Int Dynamic) a

newKey :: Hashable a => a -> Key b
newKey = Key . Hashable.hash

runCached :: Cached a -> (a, Map Int Dynamic)
runCached = flip State.runState Map.empty

evalCached :: Cached a -> a
evalCached = fst . runCached

getCache :: Typeable ty => Key ty -> Cached (Maybe ty)
getCache Key{ getKeyHash=key } = do
  map <- State.get
  return $ join $ Dynamic.fromDynamic <$> Map.lookup key map

storeCache :: Typeable ty => Key ty -> ty -> Cached ()
storeCache Key{ getKeyHash=key } val = State.modify $ Map.insert key (Dynamic.toDyn val)

cached
  :: (All Eq tys, All Hashable tys, Typeable res)
  => (HList tys -> res) -> HList tys -> Cached res
cached f args = do
  let key = newKey args
  mb <- getCache key
  case mb of
    Just res -> return res
    Nothing -> do
      let res = f args
      storeCache key res
      return res


-- Пример для экспериментов: сумма первых n чисел Фибоначчи считается долго, а повторный
-- вызов с теми же аргументами берёт ответ из кеша. Время видно в интерпретаторе после
-- команды :set +s.

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

sumNFibs :: HList '[Integer] -> Integer
sumNFibs (HCons n HNil) = sum $ List.genericTake n fibs

testCached :: Integer -> Integer -> Integer
testCached n m = evalCached do
  let f = cached sumNFibs
  x <- f (HCons n HNil)
  y <- f (HCons m HNil)
  pure (x + y)
