{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module URL where

import Prelude hiding (lookup)

import Control.Monad.IO.Class (liftIO)
import Data.Map hiding (take)
import Data.Text.Lazy hiding (empty, take)
import Network.HTTP.Types (status404)
import System.Random (randomRs, newStdGen)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)

import Web.Scotty
import Web.Scotty.Internal.Types
import Lucid

randomString :: Int -> IO String
randomString n = do
  g <- newStdGen
  return $ take n $ randomRs ('a', 'z') g

renderAnchor :: (Text, Text) -> Html ()
renderAnchor link = a_ [href_ $ toStrict $ snd link] . toHtml . fst $ link

renderForm :: ActionM ()
renderForm = do
  html . renderText $
    html_ $
      body_ $ do
        h1_ "Form"
        form_ [method_ "post", action_ "/"] $ do
          input_ [type_ "text", name_ "url"]
          button_ [type_ "submit"] "Hash"

updateStore :: MVar (Map Text Text) -> ActionT Text IO ()
updateStore store = do
  url <- param "url"
  hash <- liftIO $ randomString 8
  liftIO $ modifyMVar_ store $ \table -> return $ insert (pack hash) url table

renderList :: MVar (Map Text Text) -> ActionT Text IO ()
renderList store = do
  table <- liftIO $ readMVar store
  html . renderText $
    html_ $
      body_ $ do
        h1_ "List"
        ul_ $ traverse (li_ . renderAnchor) $ toList table

redirectHash :: MVar (Map Text Text) -> ActionT Text IO ()
redirectHash store = do
  hash <- param "hash"
  table <- liftIO $ readMVar store
  case lookup hash table of
    Nothing -> status status404
    Just url -> redirect url

main :: IO ()
main = do
  store <- newMVar (empty :: Map Text Text)
  scotty 3000 $ do
    get "/" renderForm
    post "/" $ do
      updateStore store
      redirect "/list"
    get "/list" $ renderList store
    get "/:hash" $ redirectHash store
