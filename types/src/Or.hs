{-# OPTIONS_GHC -Wall #-}

module Or where

-- import Data.Semigroup
-- import Data.Monoid
-- import Data.Functor
import Control.Applicative
-- import Control.Monad
-- import Data.Foldable
-- import Data.Traversable

data Or a b = Invalid a | Valid b deriving (Eq, Ord, Show)

instance Semigroup (Or a b) where
  (<>) (Invalid _) x = x
  (<>) x _ = x

-- Because we can... 😏
instance (Monoid a, Monoid b) => Monoid (Or a b) where
  mempty = Invalid mempty
  mappend (Valid x) (Valid x') = Valid $ mappend x x'
  mappend (Invalid x) (Invalid x') = Invalid $ mappend x x'
  mappend (Valid x) _ = Valid x
  mappend _ (Valid x) = Valid x

instance Functor (Or a) where
  fmap f (Valid x) = Valid (f x)
  fmap _ (Invalid x) = Invalid x

instance Applicative (Or a) where
  pure = Valid
  (<*>) (Valid f) x = fmap f x
  (<*>) (Invalid x)  _ = Invalid x
  liftA2 f (Valid x) (Valid x') = Valid $ f x x'
  liftA2 _ (Invalid x) _ = Invalid x
  liftA2 _ _ (Invalid x) = Invalid x

instance Monad (Or a) where
  (>>=) (Valid x) f = f x
  (>>=) (Invalid x) _ = Invalid x

instance Foldable (Or a) where
  foldMap f (Valid x) = f x
  foldMap _ _ = mempty
  foldr f y (Valid x) = f x y
  foldr _ x _ = x

instance Traversable (Or a) where
  traverse f (Valid x) = Valid <$> f x
  traverse _ (Invalid x) = pure $ Invalid x
  sequenceA (Valid x) = Valid <$> x
  sequenceA (Invalid x) = pure $ Invalid x
