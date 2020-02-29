{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Database
  ( DatabaseError
  , Database(..)
  ,createDBPool
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.AppConfig             as Cfg
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Models
import           Data.Pool
import           Data.Types
import           Database.PostgreSQL.Simple

createDBPool :: Config -> IO (Pool Connection)
createDBPool appCfg = do
  poolStripes <- require appCfg "db.pool.stripes"
  poolResourceTTL <- fromInteger <$> require appCfg "db.pool.resource_ttl"
  connInfo <- Cfg.connectionInfo appCfg
  createPool (connect connInfo) close poolStripes poolResourceTTL 1 -- maximum resources per pool

data DatabaseError =
  UnknownError
  deriving (Show)

instance Exception DatabaseError

class (MonadCatch m, MonadThrow m) =>
      Database m
  where
  createQuizzDef :: QuizzDef -> m QuizzDef
  getAllQuizzDef :: m [QuizzDef]

instance (MonadReader e m, MonadIO m, MonadThrow m, MonadCatch m, HasConnection e) => Database m where
  createQuizzDef o = do
    c <- asks getConn
    _ <- liftIO $ execute c "INSERT INTO quizz_def(id, name) VALUES (?,?)" (quizzDefId o, quizzDefName o)
    return o
  getAllQuizzDef = do
    c <- asks getConn
    liftIO $ query_ c "SELECT id, name from quizz_def"
