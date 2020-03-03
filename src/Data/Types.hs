{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Types where

import           Control.Monad.Catch
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Database.PostgreSQL.Simple (Connection)
import           Web.Scotty.Trans           (ActionT)
import Data.Pool (Pool, withResource)
import Data.AppConfig (loadCfgFile)

--config
data Env =
  Env
    { appPort    :: Int
    , version    :: T.Text
    , connection :: Connection
    }

class HasConnection e where
  getConn :: e -> Connection

instance HasConnection Env where
  getConn = connection

--app types
newtype MyApp a =
  MyApp
    { rApp :: ReaderT Env IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadThrow, MonadCatch)

type AppAction a = ActionT TL.Text MyApp a

runIO :: Pool Connection -> MyApp a -> IO a
runIO p m = do
  appConfig <- loadCfgFile
  withResource p (runReaderT (rApp m) . Env 3000 "0.1")

class (MonadIO m) => ToDTO m a d where
  toDTO :: a -> m d
  
class (MonadIO m) => FromDTO m d a where
  fromDTO ::  d -> m a
