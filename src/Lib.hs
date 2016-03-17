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

import           Database


connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

routes :: ScottyT Error ConfigM ()
routes = do
  middleware logStdoutDev
  get    "/" getIndexH

  get    "/lists" getListsH
  post   "/lists" createListH
  get    "/lists/:id" getListH
  delete "/lists/:id" deleteListH

  get    "/items" getItemsH
  post   "/items" createItemH
  get    "/items/:id" getItemH
  delete "/items/:id" deleteItemH

runApplication :: IO ()
runApplication = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
  let cfg = Config pool
  let r m = runReaderT (runConfigM m) cfg
  runSqlPool doMigration pool
  scottyT 3000 r routes
