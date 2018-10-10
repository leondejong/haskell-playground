module ListSpec where

import List

import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = listGenerator
  
listGenerator :: (Arbitrary a) => Gen (List a)
listGenerator = do
  x <- arbitrary
  frequency [(1, return Nil), (9, return $ Cons x Nil)]

prop_foldMapCons :: Integer -> Bool
prop_foldMapCons x = foldMap (+1) (Cons (Sum x) Nil) == Sum {getSum = x + 1}

prop_foldMapNil :: Integer -> Bool
prop_foldMapNil x = foldMap (+1) Nil == Sum {getSum = 0}

prop_foldrCons :: Integer -> Bool
prop_foldrCons x = foldr (+) 1 (Cons x Nil) == x + 1

prop_foldrNil :: Integer -> Bool
prop_foldrNil x = foldr (+) 1 Nil == 1

test01 :: IO ()
test01 = do
  quickCheck prop_foldMapCons
  quickCheck prop_foldMapNil
  quickCheck prop_foldrCons
  quickCheck prop_foldrNil

test02 :: IO ()
test02 = do
  let trigger :: List String
      trigger = undefined
  quickBatch $ semigroup trigger
  quickBatch $ monoid trigger

test03 :: IO ()
test03 = do
  let trigger :: List ([Integer], [Rational], [Char])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ traversable trigger

runQc :: IO ()
runQc = test01 >> test02 >> test03
