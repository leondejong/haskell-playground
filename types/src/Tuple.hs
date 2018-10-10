{-# OPTIONS_GHC -Wall #-}

module Tuple where

-- import Data.Semigroup
-- import Data.Monoid
-- import Data.Functor
import Control.Applicative
-- import Control.Monad
-- import Data.Foldable
-- import Data.Traversable

data Tuple a b = Tuple a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Tuple a b) where
  (<>) (Tuple x y) (Tuple x' y') = Tuple (x <> x') $ y <> y'

instance (Monoid a, Monoid b) => Monoid (Tuple a b) where
  mempty = Tuple mempty mempty
  mappend = (<>)

instance Functor (Tuple a) where
  fmap f (Tuple x y) = Tuple x $ f y

instance Monoid a => Applicative (Tuple a) where
  pure = Tuple mempty
  (<*>) (Tuple u f) (Tuple v x) = Tuple (mappend u v) $ f x
  liftA2 f (Tuple x y) (Tuple x' y') = Tuple (mappend x x') $ f y y'

instance Monoid a => Monad (Tuple a) where
  (>>=) (Tuple x y) f =
    let Tuple x' y' = f y
    in Tuple (mappend x x') y'

instance Foldable (Tuple a) where
  foldMap f (Tuple _ y) = f y
  foldr f y (Tuple _ x) = f x y

instance Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y
  sequenceA (Tuple x y) = Tuple x <$> y
