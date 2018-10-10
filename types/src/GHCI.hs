import Data.Semigroup
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import qualified Data.List.NonEmpty as NE

:set prompt "λ> "

-- Id

Id "x" <> Id "y"
sconcat (Id "x" NE.:| [Id "y", Id "z"])
stimes 3 Id "x"
mempty `mappend` Id "x"
mappend (Id "x") $ Id "y"
mconcat [Id "x", Id "y", Id "z"]
fmap ('x':) $ Id "y"
('x':) <$> Id "y"
Id 'y' $> 'x'
'x' <$ Id 'y'
pure 'x' :: Id Char
Id ('x':) <*> Id "y"
liftA2 (:) (Id 'x') $ Id "y"
Id 'y' *> Id 'x'
Id 'x' <* Id 'y'
return 'x' :: Id Char
Id 'x' >>= \x -> Id $ x : "y"
(\x -> Id $ x : "y") =<< Id 'x'
Id 'y' >> Id 'x'
foldMap ('x':) $ Id "y"
foldr (:) "y" $ Id 'x'
toList $ Id 'x'
null $ Id 'x'
length $ Id 'x'
elem 'x' $ Id 'x'
maximum $ Id 'x'
minimum $ Id 'x'
sum $ Id 0
product $ Id 1
traverse Just $ Id 'x'
sequenceA $ Id $ Just 'x'

-- Optional

One "x" <> One "y"
sconcat (One "x" NE.:| [One "y", One "z"])
stimes 3 One "x"
mempty `mappend` One "x"
mappend (One "x") $ One "y"
mconcat [One "x", One "y"]
fmap ('x':) $ One "y"
('x':) <$> One "y"
One 'y' $> 'x'
'x' <$ One 'y'
pure 'x' :: Optional Char
One ('x':) <*> One "y"
liftA2 (:) (One 'x') $ One "y"
One 'y' *> One 'x'
One 'x' <* One 'y'
return 'x' :: Optional Char
One 'x' >>= \x -> One $ x : "y"
(\x -> One $ x : "y") =<< One 'x'
One 'y' >> One 'x'
foldMap ('x':) $ One "y"
foldr (:) "y" $ One 'x'
toList $ One 'x'
null $ One 'x'
length $ One 'x'
elem 'x' $ One 'x'
maximum $ One 'x'
minimum $ One 'x'
sum $ One 0
product $ One 1
traverse Just $ One 'x'
sequenceA $ One $ Just 'x'

-- Or

Valid 'x' <> Valid 'y'
sconcat (Valid 'x' NE.:| [Valid 'y', Valid 'z'])
stimes 3 Valid 'x'
mempty `mappend` Valid "x"
mappend (Valid "x") $ Valid "y"
mconcat [Valid "x", Valid "y"]
fmap ('x':) $ Valid "y"
('x':) <$> Valid "y"
Valid 'y' $> 'x'
'x' <$ Valid 'y'
pure 'x' :: Or Char Char
Valid ('x':) <*> Valid "y"
liftA2 (:) (Valid 'x') $ Valid "y"
Valid 'y' *> Valid 'x'
Valid 'x' <* Valid 'y'
return 'x' :: Or Char Char
Valid 'x' >>= \x -> Valid $ x : "y"
(\x -> Valid $ x : "y") =<< Valid 'x'
Valid 'y' >> Valid 'x'
foldMap ('x':) $ Valid "y"
foldr (:) "y" $ Valid 'x'
toList $ Valid 'x'
null $ Valid 'x'
length $ Valid 'x'
elem 'x' $ Valid 'x'
maximum $ Valid 'x'
minimum $ Valid 'x'
sum $ Valid 0
product $ Valid 1
traverse Just $ Valid 'x'
sequenceA $ Valid $ Just 'x'

-- Tuple

Tuple "a" "b" <> Tuple "x" "y"
sconcat (Tuple "a" "b" NE.:| [Tuple "x" "y", Tuple "u" "v"])
stimes 3 Tuple "a" "b"
mempty `mappend` Tuple "a" "b"
mappend (Tuple "a" "b") $ Tuple "x" "y"
mconcat [Tuple "a" "b", Tuple "x" "y"]
fmap ('a':) $ Tuple "x" "y"
('a':) <$> Tuple "x" "y"
Tuple 'x' 'y' $> 'a'
'a' <$ Tuple 'x' 'y'
pure 'x' :: Tuple String Char
Tuple "a" ('b':) <*> Tuple "x" "y"
liftA2 (:) (Tuple "a" 'b') $ Tuple "x" "y"
Tuple "x" 'y' *> Tuple "a" 'b'
Tuple "a" 'b' <* Tuple "x" 'y'
return 'x' :: Tuple String Char
Tuple "a" 'b' >>= \x -> Tuple "x" $ x:"y"
(\x -> Tuple "x" $ x:"y") =<< Tuple "a" 'b'
Tuple "a" 'b' >> Tuple "x" 'y'
foldMap ('a':) $ Tuple 'x' "y"
foldr (:) "x" $ Tuple 'a' 'b'
toList $ Tuple 'x' 'y'
null $ Tuple 'x' 'y'
length $ Tuple 'x' 'y'
elem 'y' $ Tuple 'x' 'y'
maximum $ Tuple 'x' 'y'
minimum $ Tuple 'x' 'y'
sum $ Tuple 0 1
product $ Tuple 1 2
traverse Just $ Tuple 'x' 'y'
sequenceA $ Tuple 'x' $ Just 'y'

-- List

Cons 'a' $ Cons 'b' Nil <> Cons 'x' (Cons 'y' Nil)
sconcat (Cons 'a' Nil NE.:| [Cons 'x' Nil, Cons 'u' Nil])
stimes 3 Cons 'a' Nil
mempty `mappend` Cons "a" Nil
mappend (Cons "a" Nil) $ Cons "x" Nil
mconcat [Cons "a" Nil, Cons "x" Nil]
fmap ('a':) $ Cons "x" Nil
('a':) <$> Cons "x" Nil
Cons 'x' Nil $> 'a'
'a' <$ Cons 'x' Nil
pure 'a' :: List Char
Cons ('a':) Nil <*> Cons "x" Nil
liftA2 (:) (Cons 'a' Nil) (Cons "x" Nil)
Cons 'a' Nil *> Cons 'x' Nil
Cons 'a' Nil <* Cons 'x' Nil
return 'a' :: List Char
Cons 'a' Nil >>= \x -> Cons (x:"x") Nil
(\x -> Cons (x:"x") Nil) =<< Cons 'a' Nil
Cons 'a' Nil >> Cons 'x' Nil
foldMap ('a':) $ Cons "x" Nil
foldr (:) "x" $ Cons 'a' Nil
toList $ Cons 'a' Nil
null $ Cons 'a' Nil
length $ Cons 'a' Nil
elem 'a' $ Cons 'a' Nil
maximum $ Cons 'a' Nil
minimum $ Cons 'a' Nil
sum $ Cons 0 Nil
product $ Cons 1 Nil
traverse Just $ Cons 'a' Nil
sequenceA $ Cons (Just 'a') Nil
