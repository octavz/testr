{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  , routes
  ) where

import           Control.Monad.Catch        (try)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks, lift)
import           Data.AppConfig
import           Data.Database
import           Data.Default.Class         (def)
import           Data.Models
import           Data.Pool                  (Pool)
import qualified Data.Text.Lazy             as TL
import           Data.Types
import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types         (mkStatus, status201)
import           System.Random
import           Web.Scotty.Trans

routes :: Pool PG.Connection -> ScottyT TL.Text MyApp ()
routes pool = do
  get "/quizz-def" (getAllQuizzDefRoute pool)
  post "/quizz-def" (createQuizzDefRoute pool)
  get "/status" appStatus

createQuizzDefRoute :: Pool PG.Connection -> AppAction ()
createQuizzDefRoute pool = do
  uuid <- liftIO $ UUID.toText <$> randomIO
  withId <- (\o -> o {quizzDefId = Just uuid}) <$> jsonData
  res <- lift $ try $ createQuizzDef withId
  case res of
    Right _ -> status status201
    Left e  -> status $ mkStatus 500 (error $ show (e :: DatabaseError))

getAllQuizzDefRoute :: Pool PG.Connection -> AppAction ()
getAllQuizzDefRoute pool = do
  res <- lift $ try getAllQuizzDef
  case res of
    Right d -> json d
    Left e  -> status $ mkStatus 500 (error $ show (e :: DatabaseError))

appStatus = do
  v <- lift $ asks (TL.fromStrict . version)
  text ("hello world!\nversion: " <> v)

run :: IO ()
run = do
  conf <- loadCfgFile
  pool <- createDBPool conf
  scottyOptsT def (runIO pool) (routes pool)
