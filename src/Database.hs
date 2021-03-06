{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (asks)
import           Control.Monad.Reader.Class  (MonadReader)
import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Data.Aeson                  (Value, object, (.=))
import qualified Data.Text.Lazy              as T
import           Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status   (notFound404)
import           Web.Scotty.Trans

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

listWithItems :: List -> [Entity Item] -> Value
listWithItems l i = object ["name" .= listName l, "items" .= i]

getListWithItems :: Action
getListWithItems = do
  listId <- param "id"
  ml <- runDb $ DB.get $ toSqlKey listId
  l <- case ml of
    Just l -> return l
    Nothing -> next
  i <- runDb $ DB.selectList [ItemList ==. toSqlKey listId] []
  json $ listWithItems l i

doMigration :: ReaderT SqlBackend IO ()
doMigration = DB.runMigration migrateAll

runDb :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
runDb query = do
  pool <- lift $ asks getPool
  liftIO $ runSqlPool query pool
