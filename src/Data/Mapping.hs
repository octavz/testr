{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Mapping (ToDTO(..), FromDTO(..))where

import Data.Types
import Data.DTO as D
import Data.Models as M
import qualified Data.UUID                  as UUID
import System.Random
import Control.Monad.IO.Class (liftIO, MonadIO)

instance (MonadIO m) => FromDTO m D.QuizzDef M.QuizzDef where
  fromDTO  (D.QuizzDef _ name)=  do
    uuid <- liftIO $ UUID.toText <$> randomIO
    return $ M.QuizzDef uuid name

instance (MonadIO m) => ToDTO m M.QuizzDef D.QuizzDef where
  toDTO (M.QuizzDef id name) = return $ D.QuizzDef (Just id) name

instance (MonadIO m, ToDTO m a d) => ToDTO m [a] [d] where
  toDTO = mapM toDTO

instance (MonadIO m, FromDTO m d a) => FromDTO m [d] [a] where
  fromDTO = mapM fromDTO

instance (MonadIO m, ToDTO m a d) => ToDTO m (Maybe a) (Maybe d) where
  toDTO v =
    case v of
      Just an -> do
        dn <- toDTO an
        return $ Just dn
      Nothing -> return Nothing

