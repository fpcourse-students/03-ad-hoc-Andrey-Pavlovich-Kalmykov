-- | Домашка 3. Специальный (ad-hoc) полиморфизм: уровень 1, обязательные задачи.
--
-- Выданные типы, общие для уровней ('HList', 'All', 'Prop'), лежат в "Defs".
-- Решения пишутся на месте заглушек @todo@.

-- В сигнатуре showTypeList типовая переменная пока нигде не используется.
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module Level1 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import Data.Void (Void)
import Defs
import MetaUtils (todo)


-- 1.1. Словарь вместо класса
--
-- Класс типов — это словарь функций, который компилятор передаёт за нас. В этой задаче
-- словарь передаётся руками. MonoidDict a — словарь моноида на типе a: нейтральный элемент
-- и ассоциативная операция. Реализуйте:
--
--   * sumDict и productDict — два моноида на одном и том же типе Int: по сложению
--     и по умножению;
--   * pairDict — моноид на парах, собранный из моноидов на компонентах: операция
--     применяется покомпонентно;
--   * foldDict — свёртку списка: foldDict d [x, y, z] = x `combine` (y `combine` z),
--     от пустого списка — нейтральный элемент.
--
-- Заметьте: словарей для Int два, и оба законны. С классом Monoid так не выйдет — инстанс
-- у типа один, поэтому в base для этого заведены обёртки Sum и Product.

data MonoidDict a = MonoidDict
  { neutral :: a
  , combine :: a -> a -> a
  }

sumDict :: MonoidDict Int
sumDict = todo "1.1 sumDict"

productDict :: MonoidDict Int
productDict = todo "1.1 productDict"

pairDict :: MonoidDict a -> MonoidDict b -> MonoidDict (a, b)
pairDict = todo "1.1 pairDict"

foldDict :: MonoidDict a -> [a] -> a
foldDict = todo "1.1 foldDict"


-- 1.2. Список типов в строку
--
-- Постройте строку с именами типов из списка уровня типов: в квадратных скобках, через
-- запятую, без пробелов.
--
--   showTypeList @'[Int, Double]  ==  "[Int,Double]"
--   showTypeList @'[]             ==  "[]"
--
-- Имя одного типа даёт выданная функция typeName — это имя конструктора типа без аргументов:
-- typeName @(Maybe Int) == "Maybe". Понадобится свой класс типов с инстансами для пустого
-- и непустого списка, как у KnownNat из конспекта. Сигнатуру showTypeList нужно дополнить
-- ограничением; вызываться функция должна так же, как в примерах.

typeName :: forall a. Typeable a => String
typeName = tyConName $ typeRepTyCon $ typeRep $ Proxy @a

showTypeList :: forall (tys :: [Type]) . String
showTypeList = todo "1.2"


-- 1.3. Дефункционализация
--
-- Программа с функцией высшего порядка и три места, где для неё создаются предикаты:

filterHO :: (Int -> Bool) -> [Int] -> [Int]
filterHO p = \case
  [] -> []
  x : xs -> if p x then x : filterHO p xs else filterHO p xs

evens :: [Int] -> [Int]
evens = filterHO even

greaterThan :: Int -> [Int] -> [Int]
greaterThan n = filterHO (> n)

both :: (Int -> Bool) -> (Int -> Bool) -> [Int] -> [Int]
both p q = filterHO (\x -> p x && q x)

-- Избавьтесь в ней от функций высшего порядка:
--
--   * Pred — тип данных с конструктором на каждое место, где создаётся предикат; то, что
--     лямбда захватывала, становится полями конструктора;
--   * applyPred — интерпретатор этого типа;
--   * filterFO — фильтр первого порядка;
--   * isEven, isGreater, isBoth — предикаты трёх мест создания. Тесты строят предикаты
--     только этими функциями, так что имена и число конструкторов — на ваше усмотрение.
--
-- Pred обязан оставаться в классах Show и Eq: в отличие от функций, предикаты-данные можно
-- печатать и сравнивать, и тесты этим пользуются. Равные предикаты — построенные одинаково.

data Pred = PredTodo -- Заглушка: замените своими конструкторами.
  deriving (Show, Eq)

applyPred :: Pred -> Int -> Bool
applyPred = todo "1.3 applyPred"

filterFO :: Pred -> [Int] -> [Int]
filterFO = todo "1.3 filterFO"

isEven :: Pred
isEven = todo "1.3 isEven"

isGreater :: Int -> Pred
isGreater = todo "1.3 isGreater"

isBoth :: Pred -> Pred -> Pred
isBoth = todo "1.3 isBoth"


-- 1.4. Формулы как типы
--
-- Семейство Interpret переводит пропозициональную формулу в тип Haskell по соответствию
-- Карри — Говарда: конъюнкция — пара, дизъюнкция — Either, импликация — функция,
-- отрицание — функция в пустой тип Void. Терм такого типа — доказательство формулы.
--
-- Докажите равносильность: «из a следует c и из b следует c» — то же самое, что
-- «из (a или b) следует c». Чтобы увидеть, какой тип нужно населить, вычислите его
-- в интерпретаторе командой :kind! или поставьте на место решения дыру _.
-- Тесты вызывают обе половины доказательства, поэтому undefined и зацикливание
-- доказательством не считаются.

type family Interpret (prop :: Prop Type) :: Type where
  Interpret (Var v)   = v
  Interpret (Not p)   = Interpret p -> Void
  Interpret (p :/\ q) = (Interpret p, Interpret q)
  Interpret (p :\/ q) = Either (Interpret p) (Interpret q)
  Interpret (p :-> q) = Interpret p -> Interpret q

a8Like :: Interpret ((Var a :-> Var c) :/\ (Var b :-> Var c) <-> Var a :\/ Var b :-> Var c)
a8Like = todo "1.4"
