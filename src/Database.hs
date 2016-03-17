{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database where

import qualified Data.Text.Lazy                       as T
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (asks)
import           Control.Monad.Trans.Reader           (ReaderT)
import           Control.Monad.Reader.Class           (MonadReader)
import           Web.Scotty.Trans
import           Database.Persist.Postgresql          as DB
import           Models.Models

data Config = Config { getPool :: DB.ConnectionPool }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad,
              MonadIO, MonadReader Config)

type Error = T.Text
type Action = ActionT Error ConfigM ()

getIndex :: Action
getIndex = html "Hello, world"

getLists :: Action
getLists = do
  lists <- runDb $ DB.selectList [] []
  json (lists :: [Entity List])

createList :: Action
createList = do
  list <- jsonData
  listId <- runDb $ insert (list :: List)
  insertedList <- runDb $ DB.get listId
  json insertedList

getList :: Action
getList = do
  listId <- param "id"
  list <- runDb $ DB.get $ toSqlKey listId
  json (list :: Maybe List)

deleteList :: Action
deleteList = do
  listId <- param "id"
  runDb $ DB.delete (toSqlKey listId :: ListId)

getItems :: Action
getItems = do
  items <- runDb $ DB.selectList [] []
  json (items :: [Entity Item])

getItem :: Action
getItem = do
  itemId <- param "id"
  item <- runDb $ DB.get $ toSqlKey itemId
  json (item :: Maybe Item)

deleteItem :: Action
deleteItem = do
  itemId <- param "id"
  runDb $ DB.delete (toSqlKey itemId :: ItemId)

createItem :: Action
createItem = do
  item <- jsonData
  itemId <- runDb $ insert (item :: Item)
  insertedItem <- runDb $ DB.get itemId
  json insertedItem

doMigration :: ReaderT SqlBackend IO ()
doMigration = DB.runMigration migrateAll

runDb :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
runDb query = do
  pool <- lift $ asks getPool
  liftIO $ runSqlPool query pool
