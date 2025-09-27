{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC "-Wno-unused-foralls" #-}

-- | Классы типов.
-- Локальный минимум: 4б.
module Block1 where

import Data.Proxy
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.Kind
import Data.Hashable
import Data.Tagged
import Data.Foldable
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Monoid hiding (All)
import Control.Monad
import Control.Monad.State
import GHC.Generics
import GHC.TypeLits hiding (Nat, KnownNat, natVal)
import MetaUtils

data Nat = Zero | Suc Nat

data HList (types :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

type family All (c :: k -> Constraint) (types :: [k]) :: Constraint where
  All c '[] = ()
  All c (a ': as) = (c a, All c as)

-- Генерируем инстансы классов, если все компоненты хорошие.
deriving instance All Show as => Show (HList as)
deriving instance All Eq as => Eq (HList as)

hmap :: forall c {as} {result} . All c as => (forall a . c a => a -> result) -> HList as -> [result]
hmap f = \case
  HNil -> []
  HCons x xs -> f x : hmap @c f xs

instance (All Eq as, All Hashable as) => Hashable (HList as) where
  hash = hash . hmap @Hashable hash
  hashWithSalt s = foldr hashWithSalt s . hmap @Hashable hash


-- 1. (1б)
-- Постройте строчку, которая содержит список типов.

-- Перепишите эту декларацию как вам надо, сохранив API.
showTypeList :: forall (_tys :: [Type]) . String
showTypeList = todo "showTypeList"

typeName :: forall a. Typeable a => String
typeName = tyConName $ typeRepTyCon $ typeRep $ Proxy @a


-- 2. (1б)
-- Запишите reify и wonderId c помощью техники Proxy.
-- reify :: Int -> (forall n. KnownNat n => a) -> a
-- wonderId :: Int -> Int

class KnownNat (n :: Nat) where
  natVal :: Int

instance KnownNat Zero where
  natVal = 0

instance KnownNat n => KnownNat (Suc n) where
  natVal = 1 + natVal @n

wonderId :: Int -> Int
wonderId = todo "wonderId"


-- 3. (1.5б)
-- Свернём гетерогенный список .

data HSum (types :: [Type]) where
  Here :: a -> HSum (a ': as)
  There :: HSum as -> HSum (a ': as)

hfoldMap :: Monoid m => (HSum as -> m) -> HList as -> m
hfoldMap = todo "hfoldMap"

sumParticular :: HList '[Maybe Int, Int] -> Int
sumParticular = getSum . hfoldMap (todo "sumParticular")

-- Заметим, что передавать функцию от суммы всё равно,
-- что передавать произведение функций. Заставим Haskell
-- конструировать его самостоятельно.

class to <-- from where
  transform :: from -> to

hfoldMap' :: forall m as . (Monoid m, All ((<--) m) as) => HList as -> m
hfoldMap' = todo "hfoldMap'"

sumParticular' :: HList '[Maybe Int, Int] -> Int
sumParticular' = todo "sumParticular'"


-- 4. (1.5)
-- Декоратор для кеширования аргументов функции.

-- Типизированный ключ в для динамического гетерогенного хранилища.
newtype Key ty = Key { getKeyHash :: Int }
  deriving newtype (Eq, Ord)

newKey :: Hashable a => a -> Key b
newKey = todo "newKey"

-- Монада с изменяемым кешем.
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Dynamic.html
type Cached a = State (Map Int Dynamic) a

runCached :: Cached a -> (a, Map Int Dynamic)
runCached = todo "runCached"

evalCached :: Cached a -> a
evalCached = todo "evalCached"

getCache :: Typeable ty => Key ty -> Cached (Maybe ty)
getCache = todo "getCache"

storeCache :: Typeable ty => Key ty -> ty -> Cached ()
storeCache = todo "storeCache"

-- Функция, принимающая функцию от прозвольного числа аргументов
-- и возвращающая функцию поддерживающую кеширование от этих аргументов.
cached
  :: (All Eq as, All Hashable as, Typeable b)
  => (HList as -> b) -> HList as -> Cached b
cached = todo "cached"

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sumNFibs :: HList '[Integer] -> Integer
sumNFibs (HCons n HNil) = sum $ List.genericTake n fibs

-- Время работы можно проверить в ghci с помощью опции :set +s
testCached :: Integer -> Integer -> Integer
testCached n m = evalCached do
  let f = cached sumNFibs
  x <- f (HCons n HNil)
  y <- f (HCons m HNil)
  pure (x + y)
