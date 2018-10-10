module StoreSpec where

import Store

import Test.QuickCheck

getStore :: String -> Store String String
getStore x = do
  s <- get
  set $ s ++ x
  return s

concatSF :: String -> String
concatSF = (++"x")

concatSA :: Store String (String -> String)
concatSA = return $ \s -> s ++ "x"

concatSM :: String -> Store String String
concatSM s = return $ s ++ "x"

prop_store :: String -> String -> Bool
prop_store x y = compareTuple (runStore (getStore y) x) x $ x ++ y

prop_storeFunctor :: String -> String -> Bool
prop_storeFunctor x y = compareTuple (runStore (fmap concatSF $ getStore y) x) (x ++ "x") $ x ++ y

prop_storeApplicative :: String -> String -> Bool
prop_storeApplicative x y = compareTuple (runStore (concatSA <*> getStore y) x) (x ++ "x") $ x ++ y

prop_storeMonad :: String -> String -> Bool
prop_storeMonad x y = compareTuple (runStore (getStore y >>= concatSM) x) (x ++ "x") $ x ++ y

compareTuple :: (Eq a, Eq b) => (a, b) -> a -> b -> Bool
compareTuple t x y = fst t == x && snd t == y

test :: IO ()
test = do
  putStr "\n\nStore: "
  quickCheck prop_store
  putStr "\nFunctor: "
  quickCheck prop_storeFunctor
  putStr "\nApplicative: "
  quickCheck prop_storeApplicative
  putStr "\nMonad: "
  quickCheck prop_storeMonad

runQc :: IO ()
runQc = test
