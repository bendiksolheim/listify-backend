{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Lib where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Database.Persist.Postgresql (ConnectionString,
                                              withPostgresqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           Models.Models
import           Web.Scotty

connStr :: ConnectionString
connStr = "host=localhost dbname=listify user=test password=test port=5432"

getIndexH :: ActionM ()
getIndexH = html "Hello, world"

renderItemsH :: ConnectionPool -> ActionM ()
renderItemsH pool = do
  items <- liftIO $ listItems pool
  json items

routes :: ConnectionPool -> ScottyM ()
routes pool = do
  get "/" getIndexH
  get "/items" (renderItemsH pool)

runApplication :: IO ()
runApplication = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
  liftIO $ initialize pool >> scotty 3000 (routes pool)
