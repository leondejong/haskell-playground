{-# OPTIONS_GHC -Wall #-}

module List where

-- import Data.Semigroup
-- import Data.Monoid
-- import Data.Functor
import Control.Applicative
-- import Control.Monad
-- import Data.Foldable
-- import Data.Traversable

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) (Cons x xs) ys = Cons x $ xs <> ys
  (<>) _ xs = xs

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs
  fmap _ _ = mempty

instance Applicative List where
  pure x = Cons x Nil
  (<*>) (Cons f fs) xs = mappend (f <$> xs) $ fs <*> xs
  (<*>) _ _ = mempty
  liftA2 f (Cons x xs) ys = mappend (f x <$> ys) $ liftA2 f xs ys
  liftA2 _ _ _ = mempty

instance Monad List where
  (>>=) (Cons x xs) f = mappend (f x) $ xs >>= f
  (>>=) _ _ = mempty

instance Foldable List where
  foldMap f (Cons x xs) = mappend (f x) $ foldMap f xs
  foldMap _ _ = mempty
  foldr f y (Cons x xs) = f x $ foldr f y xs
  foldr _ x _ = x

instance Traversable List where
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  traverse _ _ = pure mempty
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
  sequenceA _ = pure mempty
