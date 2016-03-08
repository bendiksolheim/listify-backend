{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Lib where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger                 (runStdoutLoggingT)
import           Control.Monad.Reader                 (asks)
import           Control.Monad.Reader.Class           (MonadReader)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Control.Monad.Trans.Reader           (ReaderT, runReaderT)
import qualified Data.Text.Lazy                       as T
import           Database.Persist.Postgresql          as DB

import qualified Web.Scotty                           ()
import           Web.Scotty.Trans                     as S

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Models.Models

data Config = Config { getPool :: DB.ConnectionPool }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad,
              MonadIO, MonadReader Config)

type Error = T.Text
type Action = ActionT Error ConfigM ()

connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

getIndexH :: Action
getIndexH = html "Hello, world"

getItemsH :: Action
getItemsH = do
  items <- runDb $ DB.selectList [] []
  json (items :: [Entity Item])

getItemH :: Action
getItemH = do
  itemId <- param "id"
  item <- runDb $ DB.get $ toSqlKey itemId
  json (item :: Maybe Item)

createItemH :: Action
createItemH = do
  item <- jsonData
  itemId <- runDb $ insert (item :: Item)
  insertedItem <- runDb $ DB.get itemId
  json insertedItem

runDb :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
-- runDb :: (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- lift $ asks getPool
  liftIO $ runSqlPool query pool

routes :: ScottyT Error ConfigM ()
routes = do
  middleware logStdoutDev
  S.get  "/" getIndexH
  S.get  "/items" getItemsH
  S.post "/items" createItemH
  S.get  "/items/:id" getItemH

doMigration :: ReaderT SqlBackend IO ()
doMigration = DB.runMigration migrateAll

runApplication :: IO ()
runApplication = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
  let cfg = Config pool
  let r m = runReaderT (runConfigM m) cfg
  runSqlPool doMigration pool
  scottyT 3000 r routes
