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


-- 1.3. Из значения в тип и обратно
--
-- В конспекте reify и wonderId записаны через типовые абстракции (\ @n -> ...). Запишите их
-- техникой Proxy из главы 2: продолжение получает не тип, а значение Proxy n.
--
--   * reify n k вызывает k с типом-числом, равным n; для n <= 0 это Zero;
--   * wonderId поднимает число в тип с помощью reify и опускает обратно с помощью natVal,
--     поэтому на неотрицательных числах совпадает с id.

data Nat = Zero | Suc Nat

class KnownNat (n :: Nat) where
  natVal :: Int

instance KnownNat Zero where
  natVal = 0

instance KnownNat n => KnownNat (Suc n) where
  natVal = 1 + natVal @n

reify :: Int -> (forall n. KnownNat n => Proxy n -> a) -> a
reify _ _ = todo "1.3 reify"

wonderId :: Int -> Int
wonderId = todo "1.3 wonderId"


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
