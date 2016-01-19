{-# LANGUAGE DeriveGeneric #-}

module Models.List where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Models.Item

data List = List
    { listId :: Int -- ^ List id
    , listName :: String -- ^ List name
    , listItems :: [Item] -- ^ List items
    } deriving (Show, Generic)

instance FromJSON List
instance ToJSON List
