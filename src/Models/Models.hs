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

module Models.Models where

import           Database.Persist.Sql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item json
    name String
    count Int default=1
    deriving Show
|]

initialize :: ConnectionPool -> IO ()
initialize pool = flip runSqlPool pool $ runMigration migrateAll

listItems :: ConnectionPool -> IO [Entity Item]
--listItemsH pool = (flip runSqlPool pool $ selectList [] []) :: [Entity Item]
listItems = runSqlPool (selectList ([] :: [Filter Item]) [])
