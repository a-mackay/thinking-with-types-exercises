{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import GHC.TypeLits

main :: IO ()
main = do
    putStrLn "test"

data (a :: k1) :<< (b :: k2) -- :<< is a type constructor
    -- no corresponding data constructor so we cannot make a term-level value with type ":<<"
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type

    format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Printf text = String

    format str _ = str <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a

    format str _ = format (str <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a

    format str _ param = format (str <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf proxy = format "" proxy

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
    type Printf (String :<< a) = String -> Printf a

    format str _ param = format (str <> param) (Proxy @a)