{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Constraint, Type)

main :: IO ()
main = do
    putStrLn "test"

data MyBool
    = MyTrue -- 'MyTrue is a type with kind MyBool
    | MyFalse -- 'MyFalse is a type with kind MyBool

type Exp a
    = a -> Type
-- :kind Exp === * -> *
-- :kind Exp === Type -> Type

type family Eval (e :: Exp a) :: a
-- Therefore a is a type, of kind "Type"
-- Exp a is also a type, of kind "Type"
-- Therefore e is also a type, of kind "Type"

type instance Eval (Snd '(a, b)) = b

data Snd :: (a, b) -> Exp b
-- :k Snd???
--