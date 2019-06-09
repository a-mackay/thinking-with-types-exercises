-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative ((<$>))
import Data.Foldable (asum)
import Data.Kind (Type, Constraint)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)

main :: IO ()
main = do
    print "Test"

data Any
    = forall a . Any a

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any x) = f x

data HasShow where
    HasShow :: Show t => t -> HasShow

instance Show HasShow where
    -- show (HasShow x) = "HasShow " ++ show x
    show h = "HasShow " ++ elimHasShow show h

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow x) = f x

data Dynamic where
    Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic x) = f x

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic x) = cast x

liftD2
    :: forall a b r. (Typeable a, Typeable b, Typeable r)
    => Dynamic
    -> Dynamic
    -> (a -> b -> r)
    -> Maybe Dynamic
liftD2 d1 d2 f =
    let d :: (b -> r) -> b -> Dynamic
        d = fmap Dynamic
        g :: a -> b -> Dynamic
        g = d . f
        maybeA = fromDynamic @a d1
        maybeB = fromDynamic @b d2
    in g <$> maybeA <*> maybeB

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
    fromMaybe (error "bad pyPlus types") $ asum
        [ liftD2 @String @String a b (++)
        , liftD2 @Int @Int a b (+)
        , liftD2 @String @Int a b (\s i -> s ++ show i)
        , liftD2 @Int @String a b (\i s -> show i ++ s)
        ]

data Has (c :: Type -> Constraint) where
    Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has x) = f x

type HasShow2 = Has Show
type Dynamic2 = Has Typeable

-- | A "Constraint Synonym":
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty x = x == mempty