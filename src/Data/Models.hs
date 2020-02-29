{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Models where

import qualified Data.Text                  as T
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson.Types

data QuizzDef =
  QuizzDef
    { quizzDefId   :: Maybe T.Text
    , quizzDefName :: T.Text
    } deriving (Generic, Show)


instance FromJSON QuizzDef where
  parseJSON = withObject "QuizzDef" $ \v -> QuizzDef Nothing <$> v .: "name"

instance ToJSON QuizzDef where
  toJSON (QuizzDef id name) =
    object ["id" .= id, "name" .= name ]

instance ToRow QuizzDef where
  toRow o = [toField (quizzDefId o), toField (quizzDefName o)]

instance FromRow QuizzDef where
  fromRow = QuizzDef <$> field <*> field
