-- {-# LANGUAGE FlexibleContexts #-}

module IdSpec where

import Id

import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Eq a => EqProp (Id a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Id a) where
  arbitrary = idGenerator
  
idGenerator :: Arbitrary a => Gen (Id a)
idGenerator = do
  x <- arbitrary
  return $ Id x

idGenInt :: Gen (Id Integer)
idGenInt = idGenerator

sampleInt :: IO ()
sampleInt = sample idGenInt

prop_foldMap :: Int -> Bool
prop_foldMap x = foldMap (+1) (Id $ Sum x) == Sum {getSum = x + 1}

prop_foldr :: Int -> Bool
prop_foldr x = foldr (+) 1 (Id x) == x + 1

test01 :: IO ()
test01 = do
  quickCheck prop_foldMap
  quickCheck prop_foldr

test02 :: IO ()
test02 = do
  let trigger :: Id String
      trigger = undefined
  quickBatch $ semigroup trigger
  quickBatch $ monoid trigger

test03 :: IO ()
test03 = do
  let trigger :: Id (Int, Double, String)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ traversable trigger

runQc :: IO ()
runQc = test01 >> test02 >> test03
