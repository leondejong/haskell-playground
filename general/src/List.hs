{-# OPTIONS_GHC -Wall #-}

module List where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

toList :: Foldable t => t a -> List a
toList = foldr Cons Nil

-- toList [0, 1, 2]

mapList :: (t -> a) -> List t -> List a
mapList f (Cons x xs) = Cons (f x) (mapList f xs)
mapList _ Nil = Nil

-- mapList (+1) $ Cons 0 $ Cons 1 Nil

appendList :: List a -> List a -> List a
appendList (Cons x xs) ys = Cons x $ appendList xs ys
appendList Nil xs = xs

-- appendList (Cons 0 $ Cons 1 Nil) $ Cons 2 $ Cons 3 Nil

foldList :: (a -> b -> b) -> b -> List a -> b
foldList f y (Cons x xs) = f x (foldList f y xs)
foldList _ x Nil = x

-- foldList (+) 0 $ Cons 0 $ Cons 1 $ Cons 2 $ Cons 3 Nil

concatList :: List (List a) -> List a
concatList = foldList appendList Nil

-- concatList $ Cons (Cons 0 $ Cons 1 Nil) $ Cons (Cons 2 $ Cons 3 Nil) Nil

flatMapList :: (a -> List b) -> List a -> List b
flatMapList f xs = concatList $ mapList f xs

-- flatMapList (\x -> Cons (x+1) Nil) $ toList [0, 1, 2]

takeFromList :: Int -> List a -> List a
takeFromList n (Cons x xs) = Cons x (takeFromList (n-1) xs)
takeFromList 0 _ = Nil
takeFromList _ Nil = Nil

-- takeFromList 2 $ Cons 0 $ Cons 1 $ Cons 2 $ Cons 3 Nil

zipList :: List (a -> b) -> List a -> List b
zipList (Cons f fs) (Cons x xs) = Cons (f x) $ zipList fs xs
zipList _ Nil = Nil
zipList Nil _ = Nil

-- zipList (Cons (+1) $ Cons (*2) Nil) $ Cons 2 $ Cons 3 Nil
