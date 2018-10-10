module TupleSpec where

import Tuple

import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a, Eq b) => EqProp (Tuple a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = tupleGenerator
  
tupleGenerator :: (Arbitrary a, Arbitrary b) => Gen (Tuple a b)
tupleGenerator = do
  x <- arbitrary
  y <- arbitrary
  return $ Tuple x y

prop_foldMap :: Integer -> Bool
prop_foldMap x = foldMap (+1) (Tuple (Sum 0) $ Sum x) == Sum {getSum = x + 1}

prop_foldr :: Integer -> Bool
prop_foldr x = foldr (+) 1 (Tuple 0 x) == x + 1

test01 :: IO ()
test01 = do
  quickCheck prop_foldMap
  quickCheck prop_foldr

test02 :: IO ()
test02 = do
  let trigger :: Tuple String String
      trigger = undefined
  quickBatch $ semigroup trigger
  quickBatch $ monoid trigger

test03 :: IO ()
test03 = do
  let trigger :: Tuple ([Integer], [Rational], [Char]) ([Integer], [Rational], [Char])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ traversable trigger

runQc :: IO ()
runQc = test01 >> test02 >> test03

