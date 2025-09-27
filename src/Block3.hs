-- | TODO описание.
-- Локальный минимум: TODO б.
module Block3 where

-- type Info = [(Symbol, Info -> Type)]

-- type family Get (info :: Info) key where
--   Get ('(key, value) : rest) key = value ('(key, value) : rest)
--   Get ('(key', value) : rest) key = Get rest key ('(key', value) : rest)

-- data Expr info
--   = Const Int
--   | If (Get info "if")
--   | Match (Get info "match")

-- data If info = MkIf (Expr info) (Expr info) (Expr info)
-- data (Match info) = MkMatch (Expr info) [Branch info]
-- data Branch info = MkBranch Pattern (Expr info)
-- data Pattern = VarPattern String | CtorPattern String [Pattern]

-- type Stage1 :: Info
-- type Stage1 = '[ '("if", If), '("match", Const Void)]

-- type Stage2 :: Info
-- type Stage2 = '[ '("if", Const Void), '("match", Match)]


-- data Person = Person
--   { personName :: Tagged "name" String
--   , personPet :: Tagged "pet" Pet
--   }

-- data Pet = Pet
--   { petName :: Tagged "name" String
--   , petAge :: Tagged "age" Int
--   }

-- class Nested nested base | nested -> base where
--   pathUntil :: String

-- class JsonPathTo a where
--   pathTo :: a -> String

-- instance (Nested d base, KnownSymbol name) => JsonPathTo (d -> Tagged name a) where
--   pathTo _ = pathUntil @d ++ "." ++ symbolVal (Proxy @name)


-- instance Nested Pet Person where
--   pathUntil =


-- TODO json пути

-- TODO словари на тайплевеле
