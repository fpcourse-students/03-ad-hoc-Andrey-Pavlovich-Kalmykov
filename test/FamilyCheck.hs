{- |
Проверка «семейство типов ещё не тронуто» для задач, где ответ — семейство. Дополняет
"TypeCheck" из meta-utils: тот распознаёт заглушку 'Todo' на месте типа, а у закрытого
семейства заглушка — единственное уравнение с переменными на месте всех аргументов.
-}
module FamilyCheck (familyIsStub) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

-- | Сплайс: закрытое семейство типов выглядит как выданная заглушка — у него одно уравнение,
-- и все его образцы — переменные (например, @Eval env p = False@).
familyIsStub :: Name -> Q Exp
familyIsStub name = reify name >>= \case
  FamilyI (ClosedTypeFamilyD _ [TySynEqn _ lhs _]) _ -> lift $ all isVariable (arguments lhs)
  _ -> lift False
  where
    isVariable = \case
      VarT _ -> True
      SigT t _ -> isVariable t
      _ -> False

-- | Аргументы аппликации: @F x y@ → @[x, y]@.
arguments :: Type -> [Type]
arguments = \case
  AppT f x -> arguments f ++ [x]
  AppKindT f _ -> arguments f
  _ -> []
