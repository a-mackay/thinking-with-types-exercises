{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Kind (Constraint, Type)
import Data.Monoid ((<>))

main :: IO ()
main = do
    return ()

data HList (types :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList types -> HList (t ': types)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (x :# xs) = 1 + hLength xs

hHead :: HList (t ': ts) -> t
hHead (x :# _) = x

showBool :: HList '[a, Bool, b] -> Bool
showBool (_ :# b :# _) = b

-- instance Eq (HList '[]) where
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--     (x :# xs) == (y :# ys) = x == y && xs == ys

-- instance Ord (HList '[]) where
--     HNil <= HNil = True

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--     (x :# xs) <= (y :# ys) =
--         if x == y
--             then xs <= ys
--             else x < y

-- instance Show (HList '[]) where
--     show HNil = "[]"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--     show (x :# xs) = show x ++ ", " ++ show xs

instance (All Eq types) => Eq (HList types) where
    HNil == HNil = True
    (x :# xs) == (y :# ys) = x == y && xs == ys

instance (All Eq types, All Ord types) => Ord (HList types) where
    compare HNil HNil = EQ
    compare (x :# xs) (y :# ys) = compare x y <> compare xs ys

instance (All Show types) => Show (HList types) where
    show HNil = "[]"
    show (x :# xs) = show x ++ " :# " ++ show xs

type family AllEq (types :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (types :: [Type]) :: Constraint where
    All _ '[] = ()
    All c (t ': ts) = (c t, All c ts)
