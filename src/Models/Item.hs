{-# LANGUAGE DeriveGeneric #-}

module Models.Item where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data Item = Item
    { itemId :: Int -- ^ Item id
    , itemName :: String -- ^ Item name
    , itemCount :: Int -- ^ item count
    } deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item
