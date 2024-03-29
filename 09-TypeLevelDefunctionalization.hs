{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Constraint, Type)
import GHC.TypeLits

main :: IO ()
main = do
    putStrLn "test"

-- "Recent GHCs don't make a distinction between types and kinds
-- any more, even if you don't activate any of the relevant extensions.
-- In particular, we have Type :: Type, as you can see
-- with :k Type in GHCi."

data MyBool
    = MyTrue -- 'MyTrue is a type-level thing with kind MyBool
    | MyFalse -- 'MyFalse is a type-level thing with kind MyBool

-- A "kind synonym":
type Exp a
    = a -> Type
-- :kind Exp === * -> *
-- :kind Exp === Type -> Type

-- A closed type family has all of its equations defined in one place and
-- cannot be extended, whereas an open family can have instances spread across modules.
-- Previously we used closed type families, Eval is an open type family.
type family Eval (e :: Exp a) :: a
-- e is a type-level thing, of kind "Exp a"
-- a is a type-level thing, of kind "Type"

data Snd :: (a, b) -> Exp b
-- Snd is a type-level thing, of kind (a, b) -> Exp b
-- Due to the kind synonym, Snd is a type-level thing, of kind (a, b) -> b -> Type
-- This mirrors the type signature of the snd function, snd :: (a, b) -> b

type instance Eval (Snd '(a, b)) = b
-- Snd '(a, b) must have kind "Exp b",
-- which it does because Snd :: (a, b) -> Exp b

-- fromMaybe :: a -> Maybe a -> a
data FromMaybe :: a -> Maybe a -> Exp a

type instance Eval (FromMaybe a 'Nothing) = a
type instance Eval (FromMaybe a ('Just b)) = b

-- listToMaybe :: [a] -> Maybe a
data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (a ': as)) = 'Just a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

data FoldrList :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (FoldrList f b '[]) = b
type instance Eval (FoldrList f b (a ': as)) = Eval (FoldrList f (Eval (f a b)) as)

data Sum :: Nat -> Nat -> Exp Nat

type instance Eval (Sum x y) = x + y



-- Ad-Hoc Polymorphism

data Map :: (a -> Exp b) -> f a -> Exp (f b)

-- For lists:
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

-- For Maybe:
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))
type instance Eval (Map f 'Nothing) = 'Nothing

-- For Either:
type instance Eval (Map f ('Right b)) = 'Right (Eval (f b))
type instance Eval (Map f ('Left a)) = 'Left a

-- For tuples:
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))