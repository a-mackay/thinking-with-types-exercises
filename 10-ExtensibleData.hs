{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Type)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce

main :: IO ()
main = do
    putStrLn "test"

data OpenSum (f :: k -> Type) (ts :: [k]) where
    UnsafeOpenSum :: Int -> f t -> OpenSum f ts