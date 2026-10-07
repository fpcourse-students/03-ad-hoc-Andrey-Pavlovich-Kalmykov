{- |
Выданный код домашки 3: типы, общие для нескольких уровней. Редактировать не нужно,
решения пишутся в "Level1", "Level2" и "Level3".
-}
-- Hashable для HList объявлен через семейство All в контексте инстанса.
{-# LANGUAGE UndecidableInstances #-}
module Defs where

import Data.Hashable (Hashable (..))
import Data.Kind (Constraint, Type)

-- * Гетерогенный список

-- | Список, индексированный списком типов своих элементов (глава 2).
data HList (tys :: [Type]) where
  HNil :: HList '[]
  HCons :: ty -> HList tys -> HList (ty ': tys)

-- | Ограничение @c@ на каждый тип списка: семейство вычисляется во вложенный кортеж
-- ограничений, @All Show '[Int, Double]@ — это @(Show Int, (Show Double, ()))@.
type family All (c :: k -> Constraint) (tys :: [k]) :: Constraint where
  All c '[] = ()
  All c (ty ': tys) = (c ty, All c tys)

-- Инстансы есть, если они есть у всех элементов.
deriving instance All Show tys => Show (HList tys)
deriving instance All Eq tys => Eq (HList tys)

-- | @map@ по гетерогенному списку: функция обязана работать для любого типа с инстансом @c@.
-- Класс передаётся явно: @hmap \@Show show xs@.
hmap :: forall c {tys} {res} . All c tys => (forall ty . c ty => ty -> res) -> HList tys -> [res]
hmap f = \case
  HNil -> []
  HCons x xs -> f x : hmap @c f xs

-- | Хеш списка — хеш списка хешей его элементов.
instance (All Eq tys, All Hashable tys) => Hashable (HList tys) where
  hash = hash . hmap @Hashable hash
  hashWithSalt salt = foldr hashWithSalt salt . hmap @Hashable hash

-- * Пропозициональные формулы

-- | Синтаксис формул с переменными типа @var@.
data Prop var
  = Var var
  | Not (Prop var)
  | Prop var :\/ Prop var
  | Prop var :/\ Prop var
  | Prop var :-> Prop var

-- Приоритеты: конъюнкция связывает сильнее дизъюнкции, та — сильнее импликации,
-- слабее всех равносильность; @a :-> c :/\ b@ читается как @a :-> (c :/\ b)@.
infix 2 <->
infixr 3 :->
infixl 4 :\/
infixl 5 :/\

-- | Равносильность — две импликации.
type (<->) p q = (p :-> q) :/\ (q :-> p)
