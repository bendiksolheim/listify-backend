{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Monad.Logger                 (runStdoutLoggingT)
import           Control.Monad.Trans.Reader           (runReaderT)
import           Database.Persist.Postgresql          (ConnectionString,
                                                       createPostgresqlPool,
                                                       runSqlPool)
import           Web.Scotty.Trans
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.HTTP.Types.Status            (notFound404)

import           Database


connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

routes :: ScottyT Error ConfigM ()
routes = do
  middleware logStdoutDev
  get    "/" getIndex

  get    "/lists" getLists
  post   "/lists" createList
  get    "/lists/:id" getListWithItems
  delete "/lists/:id" deleteList

  get    "/items" getItems
  post   "/items" createItem
  get    "/items/:id" getItem
  delete "/items/:id" deleteItem

  notFound $ status notFound404

runApplication :: IO ()
runApplication = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
  let cfg = Config pool
  let r m = runReaderT (runConfigM m) cfg
  runSqlPool doMigration pool
  scottyT 3000 r routes
