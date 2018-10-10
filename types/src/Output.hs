{-# OPTIONS_GHC -Wall #-}

module Output where

import Data.Functor.Identity
import Control.Applicative

newtype OutputT o m a = OutputT { runOutputT :: m (a, o) }

type Output o = OutputT o Identity

instance Functor m => Functor (OutputT o m) where
  fmap f m = OutputT $ (fmap $ \ (a, o) -> (f a, o)) $ runOutputT m

instance (Monoid o, Applicative m) => Applicative (OutputT o m) where
  pure x  = OutputT $ pure (x, mempty)
  (<*>) f v = OutputT $ liftA2 k (runOutputT f) (runOutputT v)
    where k (a, o) (b, o') = (a b, mappend o o')

instance (Monoid o, Monad m) => Monad (OutputT o m) where
  (>>=) m k  = OutputT $ do
    (a, o) <- runOutputT m
    (b, o') <- runOutputT (k a)
    return (b, mappend o o')

output :: (Monad m) => (a, o) -> OutputT o m a
output = OutputT . return

runOutput :: Output o a -> (a, o)
runOutput = runIdentity . runOutputT

execOutput :: Output o a -> o
execOutput m = snd (runOutput m)

send :: Monad m => o -> OutputT o m ()
send o = OutputT . return $ ((), o)

observe :: Monad m => OutputT o m a -> OutputT o m (a, o)
observe m = OutputT $ do
  (a, o) <- runOutputT m
  return ((a, o), o)

observef :: Monad m => (o -> b) -> OutputT o m a -> OutputT o m (a, b)
observef f m = OutputT $ do
  (a, o) <- runOutputT m
  return ((a, f o), o)

passon :: Monad m => OutputT o m (a, o -> o) -> OutputT o m a
passon m = OutputT $ do
  ((a, f), o) <- runOutputT m
  return (a, f o)

conceal :: Monad m => (o -> o) -> OutputT o m a -> OutputT o m a
conceal f m = OutputT $ do
  (a, o) <- runOutputT m
  return (a, f o)
