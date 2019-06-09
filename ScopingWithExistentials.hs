{-# LANGUAGE RankNTypes #-}

module Main where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    putStrLn "test"

newtype ST s a
    = ST
    { unsafeRunST :: a
    }

instance Functor (ST s) where
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
    pure = ST
    ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
    ST a >>= f = seq a $ f a

newtype STRef s a
    = STRef
    { unSTRef :: IORef a
    }

newSTRef :: a -> ST s (STRef s a)
newSTRef a = pure . STRef . unsafePerformIO $ newIORef a

readSTRef :: STRef s a -> ST s a
readSTRef (STRef a) = pure . unsafePerformIO . readIORef $ a

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef a) newA = pure . unsafePerformIO $ writeIORef a newA

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef stRef f = do
    currentA <- readSTRef stRef
    writeSTRef stRef (f currentA)

runST :: (forall s. ST s a) -> a
runST st = unsafeRunST st

safeExample :: ST s String
safeExample = do
    stRef <- newSTRef "hello"
    modifySTRef stRef (++ " world")
    readSTRef stRef