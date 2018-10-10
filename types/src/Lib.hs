module Lib where

import Id
import Optional
import Or
import Tuple
import List
import Input
import Output
import Store

types :: IO ()
types = do
  let p :: Show a => a -> IO ()
      p = print
      id :: Id Char
      id = Id 'x'
      opt :: Optional Char
      opt = One 'x'
      or :: Or Char Char
      or = Valid 'x'
      tuple :: Tuple Char Char
      tuple = Tuple 'x' 'y'
      list :: List Char
      list = Cons 'x' $ Cons 'y' Nil
      input :: Input String String
      input = return "x"
      output :: Output String String
      output =  return "x"
      store :: Store String String
      store =  return "x"
  p id >> p opt >> p or >> p tuple >> p list
  p (runInput input "y") >> p (runOutput output) >> p (runStore store "y")
