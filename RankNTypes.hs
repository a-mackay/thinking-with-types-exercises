{-# LANGUAGE RankNTypes #-}

module Main where

main :: IO ()
main = putStrLn "test"

myId :: forall a. a -> a
myId x = x

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- 6.3i
-- Int -> forall a. a -> a
-- Int -> (forall a. (a -> a))
-- Rank 1
sixThreeOne :: Int -> forall a. a -> a
sixThreeOne i = id

-- 6.3ii
-- (a -> b) -> (forall c. c -> a) -> b
-- forall a b. ((a -> b) -> ((forall c. c -> a) -> b))
-- Rank 2

-- 6.3iii
-- ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
-- forall a b m z. (((forall x. m x -> b (z m x)) -> b (z m a)) -> m a)
-- Rank 3

-- | Rank 1 (caller decides a and r)
toCont :: a -> (forall r. (a -> r) -> r)
toCont x = \aToR -> aToR x

-- | Rank 2 (caller decides a, callee decides r)
fromCont :: (forall r. (a -> r) -> r) -> a
fromCont f = f id

newtype Cont a = Cont
    { unCont :: forall r. (a -> r) -> r
    }

instance Functor Cont where
    -- fmap :: (a -> b) -> f a                       -> f b
    -- fmap :: (a -> b) -> (forall r. (a -> r) -> r) -> (forall r. (b -> r) -> r)
    fmap f (Cont c) =
        Cont $ \bToR -> bToR (c f) -- mine
        -- Cont $ \bToR -> c (bToR . f) -- book

instance Applicative Cont where
    pure a = Cont $ \aToR -> aToR a
    -- (<*>) :: f (a -> b)              ->  f a              ->  f b
    -- (<*>) :: (((a -> b) -> r) -> r)  ->  ((a -> r) -> r)  ->  ((b -> r) -> r)
    -- a ::  (a -> r) -> r
    -- f :: ((a -> b) -> r) -> r

    Cont f <*> Cont a =
        Cont $ \bToR -> bToR (f a) -- mine

    -- book:
    -- Cont f <*> Cont a = Cont $ \bToR ->
    --     -- f :: r
    --     f $ \aToB ->
    --         a $ bToR . aToB -- :: r

instance Monad Cont where
    -- >>= :: m a             -> (a -> m b)             -> m b
    -- >>= :: Cont a          -> (a -> Cont b)          -> Cont b
    -- >>= :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
    Cont c >>= f = f (c id) -- mine

    -- book:
    -- Cont m >>= f = Cont $ \bToR ->
    --     m $ \a ->
    --         unCont (f a) bToR -- :: r

newtype ContT m a
    = ContT
    { unContT :: forall r. (a -> m r) -> m r
    }

instance Functor (ContT m) where
    fmap f (ContT c) = ContT $ \bToMonadR ->
        c (bToMonadR . f)

instance Applicative (ContT m) where
    pure x = ContT $ \aToMonadR -> aToMonadR x
    -- aToB :: ((a->b) -> mr) -> mr
    -- x :: (a -> mr) -> mr
    ContT aToB <*> ContT x = ContT $ \bToMonadR ->
        aToB $ \f ->
            x (bToMonadR . f)

instance Monad (ContT m) where
    -- x :: (a -> mr) -> mr
    -- f :: a -> (b -> mr) -> mr
    ContT x >>= f = ContT $ \bToMonadR ->
        x $ \a ->
            unContT (f a) bToMonadR