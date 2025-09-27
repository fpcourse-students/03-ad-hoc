-- | Семейства типов.
-- Локальный минимум: 1б.
module Block2 where

import Data.Void
import Data.Kind


-- 1. (1б)

data Prop var
  = Var var
  | Not (Prop var)
  | Prop var :\/ Prop var
  | Prop var :/\ Prop var
  | Prop var :-> Prop var

infix 2 <->
infixr 3 :->
infixl 4 :\/
infixl 5 :/\

type (<->) p q = (p :-> q) :/\ (q :-> p)

-- | Соответствие Карри-Говарда -
-- интерпретация пропозициональных формул как типов Haskell.
type family Interpret (prop :: Prop Type) :: Type where
  Interpret (Var v)   = v
  Interpret (Not p)   = Interpret p -> Void
  Interpret (p :/\ q) = (Interpret p, Interpret q)
  Interpret (p :\/ q) = Either (Interpret p) (Interpret q)
  Interpret (p :-> q) = Interpret p -> Interpret q

-- Докажите следующее утверждение логики высказываний с помощью Haskell.
-- TODO ссылка на задание
a8Like :: Interpret ((a :-> c) :/\ (b :-> c) <-> a :\/ b :-> c)
a8Like = (l2r, r2l)
  where
    l2r (f, g) = \case
      Left a -> f a
      Right b -> g b
    r2l f = (f . Left, f . Right)
