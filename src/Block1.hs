{-# LANGUAGE UndecidableInstances #-}

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

class ShowTypeList (tys :: [Type]) where
  showTypeList :: String

instance ShowTypeList '[] where
  showTypeList = "[]"

instance (Typeable ty, ShowTypeList tys) => ShowTypeList (ty ': tys) where
  showTypeList =
    let s = showTypeList @tys in
    "[" ++ typeName @ty ++ if s == "[]" then "]" else "," ++ drop 1 s

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

reify :: Int -> (forall n. KnownNat n => Proxy n -> a) -> a
reify n k
  | n <= 0 = k $ Proxy @Zero
  | otherwise = reify (n - 1) \(Proxy :: Proxy n) -> k $ Proxy @(Suc n)

wonderId :: Int -> Int
wonderId n = reify n (\(Proxy :: Proxy n) -> natVal @n)


-- 3. (1.5б)
-- Свернём гетерогенный список .

data HSum (types :: [Type]) where
  Here :: a -> HSum (a ': as)
  There :: HSum as -> HSum (a ': as)

hfoldMap :: Monoid m => (HSum as -> m) -> HList as -> m
hfoldMap f = \case
  HNil -> mempty
  HCons x xs -> f (Here x) <> hfoldMap (f . There) xs

sumParticular :: HList '[Maybe Int, Int] -> Int
sumParticular = getSum . hfoldMap \case
  Here mb -> maybe (Sum 0) Sum mb
  There (Here x) -> Sum x
  There (There x) -> case x of

-- Заметим, что передавать функцию от суммы всё равно,
-- что передавать произведение функций. Заставим Haskell
-- конструировать его самостоятельно.

class to <-- from where
  transform :: from -> to

instance Sum Int <-- Maybe Int where
  transform = \case Nothing -> mempty; Just x -> Sum x

instance Sum Int <-- Int where
  transform = Sum

hfoldMap' :: forall m as . (Monoid m, All ((<--) m) as) => HList as -> m
hfoldMap' = fold . hmap @((<--) m) transform

hfoldMap'' :: (Monoid m, All ((<--) m) as) => HList as -> m
hfoldMap'' = \case
  HNil -> mempty
  HCons x xs -> transform x <> hfoldMap' xs

sumParticular' :: HList '[Maybe Int, Int] -> Int
sumParticular' = getSum . hfoldMap'


-- 4. (1.5)
-- Декоратор для кеширования аргументов функции.

-- Типизированный ключ в для динамического гетерогенного хранилища.
newtype Key ty = Key { getKeyHash :: Int }
  deriving newtype (Eq, Ord)

newKey :: Hashable a => a -> Key b
newKey = Key . hash

-- Монада с изменяемым кешем.
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Dynamic.html
type Cached a = State (Map Int Dynamic) a

runCached :: Cached a -> (a, Map Int Dynamic)
runCached comp = runState comp Map.empty

evalCached :: Cached a -> a
evalCached comp = evalState comp Map.empty

getCache :: Typeable ty => Key ty -> Cached (Maybe ty)
getCache key = gets $ fromDynamic <=< (!? getKeyHash key)

storeCache :: Typeable ty => Key ty -> ty -> Cached ()
storeCache key value = modify (Map.insert (getKeyHash key) (toDyn value))

-- Функция, принимающая функцию от прозвольного числа аргументов
-- и возвращающая функцию поддерживающую кеширование от этих аргументов.
cached
  :: (All Eq as, All Hashable as, Typeable b)
  => (HList as -> b) -> HList as -> Cached b
cached f args = do
  let key = newKey args
  getCache key >>= \case
    Nothing -> do
      let result = f args
      storeCache key result
      pure result
    Just value ->
      pure value

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
