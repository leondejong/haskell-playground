{-# OPTIONS_GHC -Wall #-}

module Store where

import Data.Functor.Identity

newtype StoreT s m a = StoreT { runStoreT :: s -> m (a,s) }

type Store s = StoreT s Identity

instance Functor m => Functor (StoreT s m) where
  fmap f m = StoreT $ \s -> fmap (\ (a, s') -> (f a, s')) $ runStoreT m s

instance Monad m => Applicative (StoreT s m) where
  pure a = StoreT $ \s -> return (a, s)
  (<*>) (StoreT mf) (StoreT mx) = StoreT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')

instance Monad m => Monad (StoreT s m) where
  (>>=) m k = StoreT $ \s -> do
    (a, s') <- runStoreT m s
    runStoreT (k a) s'

store :: Monad m => (s -> (a, s)) -> StoreT s m a
store f = StoreT (return . f)

runStore :: Store s a -> s -> (a, s)
runStore m = runIdentity . runStoreT m

evalStore :: Store s a -> s -> a
evalStore m s = fst (runStore m s)

execStore :: Store s a -> s -> s
execStore m s = snd (runStore m s)

get :: Monad m => StoreT s m s
get = StoreT $ return . (\s -> (s, s))

set :: Monad m => s -> StoreT s m ()
set s = StoreT $ return . (\_ -> ((), s))

getf :: Monad m => (s -> a) -> StoreT s m a
getf f = StoreT $ return . (\s -> (f s, s))

transform :: Monad m => (s -> s) -> StoreT s m ()
transform f = StoreT $ return . (\s -> ((), f s))
