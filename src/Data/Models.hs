{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.Models where

import qualified Data.Text                  as T
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

data QuestionSet = 
  QuestionSet{
    questionSet_id :: Maybe T.Text
    , questionSet_name :: Maybe T.Text
  } deriving (Generic, Show)

data QuizzDefQuestionSet = 
  QuizzDefQuestionSet
    {
      quizzDefQuestionSet_quizzDefId :: T.Text,
      quizzDefQuestionSet_questionSetId :: T.Text,
      quizzDefQuestionSet_count :: T.Text
    }  deriving (Generic, Show)
    
data QuizzDef =
  QuizzDef
    { quizzDef_id   :: T.Text
    , quizzDef_name :: T.Text
    } deriving (Generic, Show)


instance ToRow QuizzDef where
  toRow o = [toField (quizzDef_id o), toField (quizzDef_name o)]

instance FromRow QuizzDef where
  fromRow = QuizzDef <$> field <*> field
