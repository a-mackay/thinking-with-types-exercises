{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Typeable

main :: IO ()
main = putStrLn "Hi"

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
    fmap f (T1 g) = T1 (f . g)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
    fmap f (T5 g) = T5 (\bToI -> g $ bToI . f)

typeName :: forall a . Typeable a => String
-- typeName = show . typeRep $ (Proxy :: Proxy a)
typeName = show . typeRep $ Proxy @a