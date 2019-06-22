{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (fst)

main :: IO ()
main = do
    putStrLn "test"

data Fst a b
    = Fst (a, b)
    deriving (Eq, Show)

class Eval l t | l -> t where
    eval :: l -> t

instance Eval (Fst a b) a where
    eval (Fst (a, b)) = a

data ListToMaybe a
    = ListToMaybe [a]
    deriving (Eq, Show)

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe []) = Nothing
    eval (ListToMaybe (x : _)) = Just x

data MapList a b
    = MapList (a -> b) [a]

instance Eval (MapList a b) [b] where
    eval (MapList _ []) = []
    eval (MapList f (x : xs)) = f x : eval (MapList f xs)