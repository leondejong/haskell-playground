class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: Integral b => b -> a -> a

-- Associativity
(x <> y) <> z = x <> (y <> z)

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty, mappend #-}

-- Associativity
mappend x (mappend y z) = mappend (mappend x y) z

-- Left identity
mappend mempty x = x

-- Right identity
mappend x mempty = x

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

-- Identity:
fmap id = id

-- Composition:
fmap (f . g) = (fmap f) . (fmap g)

-- Structure preservation
fmap :: Functor f => (a -> b) -> f a -> f b

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

-- Identity:
pure id <*> v = v

-- Homomorphism:
pure f <*> pure x = pure (f x)

-- Interchange:
u <*> pure y = pure ($ y) <*> u

-- Composition:
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}

-- Left identity:
return x >>= f = f x

-- Right identity:
m >>= return = m

-- Associativity:
(m >>= f) >>= g == m >>= (\x -> f x >>= g)

class Foldable (t :: * -> *) where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}

-- Lawless

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}

-- Naturality
t . traverse f = traverse (t . f)
t . sequenceA = sequenceA . fmap t

-- Identity
traverse Identity = Identity
sequenceA . fmap Identity = Identity

-- Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
