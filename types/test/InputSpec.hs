module InputSpec where

import Input

import Test.QuickCheck

getInput :: Input String String
getInput = do
  s <- fetch
  return s

concatIF :: String -> String
concatIF = (++"x")

concatIA :: Input String (String -> String)
concatIA = return $ \s -> s ++ "x"

concatIM :: String -> Input String String
concatIM s = return $ s ++ "x"

prop_input :: String -> Bool
prop_input x = runInput getInput x == x

prop_inputFunctor :: String -> Bool
prop_inputFunctor x = runInput (fmap concatIF getInput) x == x ++ "x"

prop_inputApplicative :: String -> Bool
prop_inputApplicative x = runInput (concatIA <*> getInput) x == x ++ "x"

prop_inputMonad :: String -> Bool
prop_inputMonad x = runInput (getInput >>= concatIM) x == x ++ "x"

test :: IO ()
test = do
  putStr "\n\nInput: "
  quickCheck prop_input
  putStr "\nFunctor: "
  quickCheck prop_inputFunctor
  putStr "\nApplicative: "
  quickCheck prop_inputApplicative
  putStr "\nMonad: "
  quickCheck prop_inputMonad

runQc :: IO ()
runQc = test
