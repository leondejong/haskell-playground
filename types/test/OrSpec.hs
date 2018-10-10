module OrSpec where

import Or

import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a, Eq b) => EqProp (Or a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGenerator
  
orGenerator :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGenerator = do
  x <- arbitrary
  y <- arbitrary
  frequency [(1, return $ Invalid x), (1, return $ Valid y)]

prop_foldMapFirst :: Integer -> Bool
prop_foldMapFirst x = foldMap (+1) (Invalid (Product x)) == Product {getProduct = 1}

prop_foldMapSecond :: Integer -> Bool
prop_foldMapSecond x = foldMap (+1) (Valid (Product x)) == Product {getProduct = x + 1}

prop_foldrFirst :: Integer -> Bool
prop_foldrFirst x = foldr (+) 1 (Invalid x) == 1

prop_foldrSecond :: Integer -> Bool
prop_foldrSecond x = foldr (+) 1 (Valid x) == x + 1

test01 :: IO ()
test01 = do
  quickCheck prop_foldMapFirst
  quickCheck prop_foldMapSecond
  quickCheck prop_foldrFirst
  quickCheck prop_foldrSecond

test02 :: IO ()
test02 = do
  let trigger :: Or String String
      trigger = undefined
  quickBatch $ semigroup trigger
  quickBatch $ monoid trigger

test03 :: IO ()
test03 = do
  let trigger :: Or ([Integer], [Rational], [Char]) ([Integer], [Rational], [Char])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ traversable trigger

runQc :: IO ()
runQc = test01 >> test02 >> test03
