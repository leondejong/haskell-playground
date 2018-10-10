{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import GHC.Generics
import Control.Monad.IO.Class
import System.Random

import Data.Maybe
import Data.ByteString hiding (take, filter)
import Data.Aeson hiding (json)

import Web.Scotty
import Web.Scotty.Internal.Types

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Database.Redis as R

data Item = Item
  { key :: T.Text
  , name :: T.Text
  , content :: T.Text
  } | Failure String
  deriving (Generic, Show)

instance ToJSON Item
instance FromJSON Item

type List = [Item]

list :: List
list = 
  [ Item {key = "1", name = "Name 01", content = "Content 01"}
  , Item {key = "2", name = "Name 02", content = "Content 02"}
  , Item {key = "3", name = "Name 03", content = "Content 03"}
  ]

randomKey :: Int -> IO T.Text
randomKey n = do
  g <- newStdGen
  return $ T.pack $ take n $ randomRs ('a', 'z') g

toList :: Maybe ByteString -> List
toList mbs = (fromMaybe []) $ decode $ B.fromStrict $ (fromMaybe "") mbs

storeList :: List -> IO List
storeList list' = do
  connection <- R.checkedConnect R.defaultConnectInfo
  R.runRedis connection $ do
    response <- R.set "list" $ B.toStrict $ encode list'
    case response of
      Left _ -> return [Failure "failure"]
      Right _ -> return list'

retrieveList :: IO List
retrieveList = do
  connection <- R.checkedConnect R.defaultConnectInfo
  R.runRedis connection $ do
    response <- R.get "list"
    case response of
      Left _ -> return [Failure "failure"]
      Right mbs -> return $ toList mbs

removeList :: IO List
removeList = storeList []

storeItem :: Item -> IO List
storeItem item = do
  list' <- liftIO retrieveList
  list'' <- return $ filter (\item' -> key item /= key item') list'
  list''' <- liftIO $ storeList $ list'' ++ [item]
  return list'''

retrieveItem :: T.Text -> IO Item
retrieveItem id' = do
  list' <- liftIO retrieveList
  return $ filter (\item -> id' == key item) list' !! 0

removeItem :: T.Text -> IO List
removeItem id' = do
  list' <- liftIO retrieveList
  list'' <- return $ filter (\item -> id' /= key item) list'
  list''' <- liftIO $ storeList $ list''
  return list'''

getList :: ActionT T.Text IO ()
getList = do
  list' <- liftIO retrieveList
  json list'

getItem :: ActionT T.Text IO ()
getItem = do
  id' <- param "id"
  item <- liftIO $ retrieveItem id'
  json $ item

saveItem :: T.Text -> ActionT T.Text IO ()
saveItem id' = do
  name' <- param "name"
  content' <- param "content"
  response <- liftIO $ storeItem $ Item id' name' content'
  json $ response

addItem :: ActionT T.Text IO ()
addItem = do
  id' <- liftIO $ randomKey 8
  saveItem id'

updateItem :: ActionT T.Text IO ()
updateItem = do
  id' <- param "id"
  saveItem id'

deleteItem :: ActionT T.Text IO ()
deleteItem = do
  id' <- param "id"
  response <- liftIO $ removeItem id'
  json $ response

main :: IO ()
main = do
  scotty 3000 $ do
    get "/list" $ getList
    get "/item/:id" $ getItem
    post "/item" $ addItem
    put "/item" $ updateItem
    delete "/item" $ deleteItem
