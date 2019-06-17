{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum(..), Product(..))

main :: IO ()
main = do
    putStrLn "test"

newtype Reverse a
    = Reverse
    { getReverse :: a
    } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
    compare (Reverse a) (Reverse b) = compare b a

-- What is the role signature of Either a b ?
-- Either (a :: representational) (b :: representational)

-- What is the role signature of Proxy a ?
-- Proxy (a :: phantom)

type family IntToBool a where
    IntToBool Int = Bool
    IntToBool a = a

data BinSearchTree v
    = Empty
    | Branch (BinSearchTree v) v (BinSearchTree v)
    deriving (Show)

type role BinSearchTree nominal