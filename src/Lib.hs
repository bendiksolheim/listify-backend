{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Database.Persist.Postgresql (ConnectionString,
                                              withPostgresqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           Models.Models
import           Web.Scotty

connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

getIndexH :: ActionM ()
getIndexH = html "Hello, world"

getItemsH :: ConnectionPool -> ActionM ()
getItemsH pool = do
  items <- liftIO $ listItems pool
  json items

getItemH :: ConnectionPool -> ActionM ()
getItemH pool = do
  itemId <- param "id"
  item   <- liftIO $ getItem pool itemId
  json item

createItemH :: ConnectionPool -> ActionM ()
createItemH pool = do
  item :: Item <- jsonData
  insertedItem <- liftIO $ insertItem pool item
  json insertedItem

routes :: ConnectionPool -> ScottyM ()
routes pool = do
  get  "/" getIndexH
  get  "/items" (getItemsH pool)
  post "/items" (createItemH pool)
  get  "/items/:id" (getItemH pool)

runApplication :: IO ()
runApplication =
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
  liftIO $ initialize pool >> scotty 3000 (routes pool)
