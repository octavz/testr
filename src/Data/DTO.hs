{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.DTO where

import qualified Data.Text                  as T
import GHC.Generics (Generic)
import Data.Aeson.Types

data QuizzDefQuestionSet =
  QuizzDefQuestionSet
  {
    quizzDefQuestionSet_id :: T.Text,
    quizzDefQuestionSet_count :: Int
  } deriving(Generic, Show)
  
data QuizzDef =
  QuizzDef
    {
     quizzDefDTO_id :: Maybe T.Text,
     quizzDefDTO_name :: T.Text
    } deriving (Generic, Show)

instance FromJSON QuizzDef where
  parseJSON = withObject "QuizzDefDTO" $ \v -> QuizzDef Nothing <$>
    v .: "name"

instance ToJSON QuizzDef where
  toJSON (QuizzDef id name ) =
    object ["id" .= id, "name" .= name ]

