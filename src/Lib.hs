{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Control.Monad.Logger                 (runStdoutLoggingT)
import           Control.Monad.Trans.Reader           (runReaderT)
import           Database.Persist.Postgresql

import qualified Web.Scotty                           ()
import           Web.Scotty.Trans                     as S

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Database


connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

routes :: ScottyT Error ConfigM ()
routes = do
  middleware logStdoutDev
  S.get    "/" getIndexH
  S.get    "/lists" getListsH
  S.post   "/lists" createListH
  S.get    "/lists/:id" getListH
  S.get    "/items" getItemsH
  S.post   "/items" createItemH
  S.get    "/items/:id" getItemH
  S.delete "/items/:id" deleteItemH

runApplication :: IO ()
runApplication = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
  let cfg = Config pool
  let r m = runReaderT (runConfigM m) cfg
  runSqlPool doMigration pool
  scottyT 3000 r routes
