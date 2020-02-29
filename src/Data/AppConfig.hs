{-# LANGUAGE OverloadedStrings #-}

module Data.AppConfig where

import qualified Data.Configurator          as Cfg
import           Data.Configurator.Types
import qualified Database.PostgreSQL.Simple as PG

loadCfgFile :: IO Config
loadCfgFile = Cfg.load ["app.cfg"]

connectionInfo :: Config -> IO PG.ConnectInfo
connectionInfo appCfg = do
  username <- Cfg.require appCfg "db.user"
  password <- Cfg.require appCfg "db.password"
  host <- Cfg.require appCfg "db.host"
  dbname <- Cfg.require appCfg "db.dbname"
  port <- Cfg.require appCfg "db.port"
  pure $
    PG.defaultConnectInfo
      { PG.connectUser = username
      , PG.connectPassword = password
      , PG.connectHost = host
      , PG.connectPort = port
      , PG.connectDatabase = dbname
      }

