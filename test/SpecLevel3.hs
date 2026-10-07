module SpecLevel3 where

import Defs
import FamilyCheck
import Level3
import Test.Prelude
import TypeCheck

tests :: NamedTests
tests = nameTests 3
  [ testEval
  ]

-- | Формула @(a \/ b) -> (~a -> b)@ — тавтология: во всех окружениях она истинна.
type Tautology = (Var "a" :\/ Var "b") :-> (Not (Var "a") :-> Var "b")

-- | Значения семейства сравниваются во время выполнения (см. "TypeCheck"), поэтому неверный
-- ответ — проваленный тест, а не ошибка сборки. Выданная заглушка распознаётся по форме.
testEval :: Test
testEval = if $(familyIsStub ''Eval) then TestCase (todo "3.1") else TestList
  [ typeIs @True @(Eval '[ '("a", True) ] (Var "a")) "3.1 переменная"
  , typeIs @False @(Eval '[ '("a", False), '("b", True) ] (Var "a")) "3.1 первая переменная окружения"
  , typeIs @True @(Eval '[ '("a", False), '("b", True) ] (Var "b")) "3.1 вторая переменная окружения"
  , typeIs @False @(Eval '[ '("a", True) ] (Not (Var "a"))) "3.1 отрицание"
  , typeIs @True @(Eval '[ '("a", False), '("b", True) ] (Var "a" :\/ Var "b")) "3.1 дизъюнкция"
  , typeIs @False @(Eval '[ '("a", False), '("b", True) ] (Var "a" :/\ Var "b")) "3.1 конъюнкция"
  , typeIs @False @(Eval '[ '("a", True), '("b", False) ] (Var "a" :-> Var "b")) "3.1 импликация"
  , typeIs @True @(Eval '[ '("a", False), '("b", False) ] (Var "a" :-> Var "b")) "3.1 импликация из лжи"
  , typeIs @True @(Eval '[ '("a", False), '("b", False) ] Tautology) "3.1 тавтология ff"
  , typeIs @True @(Eval '[ '("a", False), '("b", True) ] Tautology) "3.1 тавтология ft"
  , typeIs @True @(Eval '[ '("a", True), '("b", False) ] Tautology) "3.1 тавтология tf"
  , typeIs @True @(Eval '[ '("a", True), '("b", True) ] Tautology) "3.1 тавтология tt"
  ]
