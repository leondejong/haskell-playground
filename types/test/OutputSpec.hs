module OutputSpec where

import Output

import Test.QuickCheck

setOutput :: String -> Output String String
setOutput s = do
  send s
  return s

concatOF :: String -> String
concatOF = (++"x")

concatOA :: Output String (String -> String)
concatOA = return $ \s -> s ++ "x"

concatOM :: String -> Output String String
concatOM s = return $ s ++ "x"

prop_output :: String -> Bool
prop_output x = compareTuple (runOutput $ setOutput x) x x

prop_outputFunctor :: String -> Bool
prop_outputFunctor x = compareTuple (runOutput (fmap concatOF $ setOutput x)) (x ++ "x") x

prop_outputApplicative :: String -> Bool
prop_outputApplicative x = compareTuple (runOutput (concatOA <*> setOutput x)) (x ++ "x") x

prop_outputMonad :: String -> Bool
prop_outputMonad x = compareTuple (runOutput (setOutput x >>= concatOM)) (x ++ "x") x

compareTuple :: (Eq a, Eq b) => (a, b) -> a -> b -> Bool
compareTuple t x y = fst t == x && snd t == y

test :: IO ()
test = do
  putStr "\n\nOutput: "
  quickCheck prop_output
  putStr "\nFunctor: "
  quickCheck prop_outputFunctor
  putStr "\nApplicative: "
  quickCheck prop_outputApplicative
  putStr "\nMonad: "
  quickCheck prop_outputMonad

runQc :: IO ()
runQc = test
