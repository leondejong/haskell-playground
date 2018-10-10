{-# OPTIONS_GHC -Wall #-}

module RWST where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Map.Lazy
import Prelude hiding (lookup)

type Config = Map String String

config :: Config
config = fromList [("host", "127.0.0.1"), ("port", "8080")]

fetch :: Monad m => String -> ReaderT Config m (Maybe String)
fetch key = do
  conf <- ask
  return (lookup key conf)

output :: (Monad m, MonadTrans t, Monoid w) => w -> t (WriterT w m) ()
output = lift . tell

input ::  Monad m => String -> ReaderT Config m [Char]
input key = do
  value <- fetch key
  return (maybe "-" id value)

getConfig :: ReaderT Config (WriterT String IO) ()
getConfig = do
  host <- input "host"
  port <- input "port"
  output "\n-- Config --"
  output $ "\nHost: " ++ host
  output $ "\nPort: " ++ port
  output $ "\n"
  return ()

addConfig :: String -> String -> StateT Config IO ()
addConfig key value = do
  conf <- get
  put $ insert key value conf
  liftIO $ putStr "Added: " >> print (key, value)

main :: IO ()
main = do
  execWriterT (runReaderT getConfig config) >>= putStrLn
  execStateT (addConfig "scheme" "https") config >>= print >> putStr "\n"
