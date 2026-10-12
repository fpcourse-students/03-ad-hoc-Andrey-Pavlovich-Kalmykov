-- | Домашка 3. Специальный (ad-hoc) полиморфизм: уровень 3.

-- Семейства типов в 3.1 вызывают друг друга во вложенных аппликациях; GHC требует для этого
-- явно разрешить определения, завершимость которых он проверить не может.
{-# LANGUAGE UndecidableInstances #-}
module Level3 where

import Defs
import GHC.TypeLits (Symbol)


-- 3.1. Формулы вычисляются в типах
--
-- Реализуйте семейство типов Eval: значение формулы Prop над переменными-строками
-- в окружении env — списке пар (имя, значение) уровня типов.
--
--   Eval '[ '("a", True), '("b", False) ] (Var "a" :-> Var "b")  ~  False
--
-- Скорее всего, понадобятся вспомогательные семейства: поиск переменной в окружении
-- и логические операции над Bool уровня типов. Проверять себя удобно в интерпретаторе
-- командой :kind!.
--
-- Тесты подставляют полные окружения (все переменные формулы определены); на них семейство
-- обязано вычисляться до True или False, иначе тесты не соберутся.

type family Lookup (name :: Symbol) (env :: [(Symbol, Bool)]) :: Bool where
  Lookup name ('(name, val) ': rest) = val
  Lookup name ('(other, val) ': rest) = Lookup name rest

type family Not' (b :: Bool) :: Bool where
  Not' 'True  = 'False
  Not' 'False = 'True

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And _     _     = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or _      _      = 'True

type family Impl (a :: Bool) (b :: Bool) :: Bool where
  Impl 'True 'False = 'False
  Impl _     _      = 'True


type family Eval (env :: [(Symbol, Bool)]) (p :: Prop Symbol) :: Bool where
  Eval env (Var x)    = Lookup x env
  Eval env (Not p)    = Not' (Eval env p)
  Eval env (p :/\ q)  = And (Eval env p) (Eval env q)
  Eval env (p :\/ q)  = Or (Eval env p) (Eval env q)
  Eval env (p :-> q)  = Impl (Eval env p) (Eval env q)

