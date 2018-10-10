{-# OPTIONS_GHC -Wall #-}

module Id where

-- import Data.Semigroup
-- import Data.Monoid
-- import Data.Functor
import Control.Applicative
-- import Control.Monad
-- import Data.Foldable
-- import Data.Traversable

newtype Id a = Id a deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Id a) where
  (<>) (Id x) (Id x') = Id $ x <> x'

instance (Monoid a) => Monoid (Id a) where
  mempty = Id mempty
  mappend = (<>)

instance Functor Id where
  fmap f (Id x) = Id $ f x

instance Applicative Id where
  pure = Id
  (<*>) (Id f) (Id x) = Id $ f x
  liftA2 f (Id x) (Id x') = pure $ f x x'

instance Monad Id where
  (>>=) (Id x) f = f x

instance Foldable Id where
  foldMap f (Id x) = f x
  foldr f x' (Id x) = f x x'

instance Traversable Id where
  traverse f (Id x) = Id <$> f x
  sequenceA (Id x) = Id <$> x
