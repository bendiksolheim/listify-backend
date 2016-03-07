{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Models.Models where

import           Database.Persist.Sql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item json
    name String
    count Int default=1
    deriving Show
|]

intToKey :: ToBackendKey SqlBackend a => Integer -> Key a
intToKey intId = toSqlKey $ fromIntegral intId

initialize :: ConnectionPool -> IO ()
initialize pool = flip runSqlPool pool $ runMigration migrateAll

listItems :: ConnectionPool -> IO [Entity Item]
listItems = runSqlPool (selectList ([] :: [Filter Item]) [])

insertItem :: ConnectionPool -> Item -> IO (Maybe Item)
insertItem pool item = do
  itemId <- runSqlPool (insert item) pool
  runSqlPool (get itemId) pool

getItem :: ConnectionPool -> Integer -> IO (Maybe Item)
getItem pool intId = flip runSqlPool pool . get . intToKey $ intId
