{-# OPTIONS_GHC -Wall #-}

module Optional where

-- import Data.Semigroup
-- import Data.Monoid
-- import Data.Functor
import Control.Applicative
-- import Control.Monad
-- import Data.Foldable
-- import Data.Traversable

data Optional a = None | One a deriving (Eq, Ord, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  (<>) (One x) (One x') = One $ x <> x'
  (<>) x None = x
  (<>) None x = x

instance (Semigroup a) => Monoid (Optional a) where
  mempty = None
  mappend = (<>)

instance Functor Optional where
  fmap f (One x) = One $ f x
  fmap _ _ = None

instance Applicative Optional where
  pure = One
  (<*>) (One f) x = fmap f x
  (<*>) _ _ = None
  liftA2 f (One x) (One x') = One $ f x x'
  liftA2 _ _ _ = None

instance Monad Optional where
  (>>=) (One x) f = f x
  (>>=) _ _ = None

instance Foldable Optional where
  foldMap f (One x) = f x
  foldMap _ _ = mempty
  foldr f y (One x) = f x y
  foldr _ x _ = x

instance Traversable Optional where
  traverse f (One x) = One <$> f x
  traverse _ _ = pure None
  sequenceA (One x) = One <$> x
  sequenceA _ = pure None
