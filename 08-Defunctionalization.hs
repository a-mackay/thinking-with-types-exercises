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

-- fst :: (a, b) -> a

data Fst a b
    = Fst (a, b)
    deriving (Show)

class Eval label t | label -> t where
    eval :: label -> t

instance Eval (Fst a b) a where
    eval (Fst (a, b)) = a

-- listToMaybe :: [a] -> Maybe a

data ListToMaybe a
    = ListToMaybe [a]
    deriving (Show)

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe []) = Nothing
    eval (ListToMaybe (a : _)) = Just a

-- mapList :: (a -> b) -> [a] -> [b]

-- straightforward approach:
data MapList a dfb
    = MapList (a -> dfb) [a]

-- instance Eval (MapList a dfb) [dfb] where
--     eval (MapList f []) = []
--     eval (MapList f (a : as)) = f a : eval (MapList f as)

-- book approach (further apply 'eval's to remove nested structure):
instance Eval dfb dft => Eval (MapList a dfb) [dft] where
    eval (MapList f []) = [] -- :: [dft] now instead of [dfb]
    eval (MapList f (a : as)) = eval (f a) : eval (MapList f as) -- every list elem now has type 'dft'