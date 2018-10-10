module OptionalSpec where

import Optional

import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = optionalGenerator
  
optionalGenerator :: (Arbitrary a) => Gen (Optional a)
optionalGenerator = do
  x <- arbitrary
  frequency [(1, return $ One x), (1, return None)]

prop_foldMapNone :: Integer -> Bool
prop_foldMapNone x = foldMap (+1) None == Product {getProduct = 1}

prop_foldMapOne :: Integer -> Bool
prop_foldMapOne x = foldMap (+1) (One (Product x)) == Product {getProduct = x + 1}

prop_foldrNone :: Integer -> Bool
prop_foldrNone x = foldr (+) 1 None == 1

prop_foldrOne :: Integer -> Bool
prop_foldrOne x = foldr (+) 1 (One x) == x + 1

test01 :: IO ()
test01 = do
  quickCheck prop_foldMapNone
  quickCheck prop_foldMapOne
  quickCheck prop_foldrNone
  quickCheck prop_foldrOne

test02 :: IO ()
test02 = do
  let trigger :: Optional String
      trigger = undefined
  quickBatch $ semigroup trigger
  quickBatch $ monoid trigger

test03 :: IO ()
test03 = do
  let trigger :: Optional ([Integer], [Rational], [Char])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ traversable trigger

runQc :: IO ()
runQc = test01 >> test02 >> test03
