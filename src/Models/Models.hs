{-# LANGUAGE FlexibleContexts           #-}
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
{-# LANGUAGE DeriveGeneric              #-}

module Models.Models where

import           GHC.Generics
import           Database.Persist.TH
import           Data.Aeson
import           Database.Persist.Postgresql (Entity)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
List json
    name String
    deriving Show
Item json
    name String
    count Int default=1
    list ListId
    deriving Show
|]

data ListWithItems = ListWithItems
  { list :: Maybe List
  , items :: [Entity Item]
  } deriving (Show, Generic)

instance ToJSON ListWithItems
