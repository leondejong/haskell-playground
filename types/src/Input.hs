{-# OPTIONS_GHC -Wall #-}

module Input where

import Data.Functor.Identity

newtype InputT i m a = InputT { runInputT :: i -> m a }

type Input i = InputT i Identity

instance Functor m => Functor (InputT i m) where
  fmap f m = InputT $ (fmap f) . runInputT m

instance Applicative m => Applicative (InputT i m) where
  pure = (InputT . const . pure)
  (<*>) f v = InputT $ \i -> runInputT f i <*> runInputT v i

instance Monad m => Monad (InputT i m) where
  (>>=) m f = InputT $ \i -> do
    a <- runInputT m i
    runInputT (f a) i

input :: Monad m => (i -> a) -> InputT i m a
input f = InputT (return . f)

runInput :: Input i a -> i -> a
runInput m = runIdentity . runInputT m

fetch :: Monad m => InputT i m i
fetch = InputT return

fetchf :: Monad m => (i -> a) -> InputT i m a
fetchf f = InputT (return . f)

scope :: (i -> i) -> InputT i m a -> InputT i m a
scope f m = InputT $ runInputT m . f
