{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( run
  , routes
  ) where

import           Control.Monad.Catch        (try, MonadCatch)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks, lift)
import           Data.AppConfig
import           Data.Database
import           Data.Default.Class         (def)
import qualified Data.DTO                   as D
import qualified Data.Models                   as M
import           Data.Mapping
import           Data.Pool                  (Pool)
import qualified Data.Text.Lazy             as TL
import           Data.Types
import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types         (mkStatus, status201)
import           System.Random
import           Web.Scotty.Trans
import Data.Aeson (ToJSON)

routes :: Pool PG.Connection -> ScottyT TL.Text MyApp ()
routes pool = do
  get "/quizz-def" getAllQuizzDefRoute
  post "/quizz-def" (createQuizzDefRoute pool)
  get "/status" appStatus

createQuizzDefRoute :: Pool PG.Connection -> AppAction ()
createQuizzDefRoute pool = do
  uuid <- liftIO $ UUID.toText <$> randomIO
  dto <- jsonData :: AppAction D.QuizzDef
  withId <- liftIO $ fromDTO dto
  res <- lift $ try $ createQuizzDef withId
  case res of
    Right _ -> status status201
    Left e  -> status $ mkStatus 500 (error $ show (e :: DatabaseError))

--getAllQuizzDefRoute :: AppAction ()
--getAllQuizzDefRoute = do
--  res <- lift (try getAllQuizzDef :: MyApp (Either DatabaseError [M.QuizzDef]))
--  case res of
--    Right a -> liftIO (toDTO a :: (IO [D.QuizzDef])) >>= json
--    Left e -> status $ mkStatus 500 (error $ show (e :: DatabaseError))

getAllQuizzDefRoute ::  AppAction ()
getAllQuizzDefRoute = attempt getAllQuizzDef (toDTO :: [M.QuizzDef] -> MyApp [D.QuizzDef])

attempt :: (MonadCatch m, ToJSON a) => m t -> (t -> m a) -> ActionT TL.Text m ()
attempt f t = do
  res <- lift (try f)
  case res of
    Right r -> lift (t r) >>= json
    Left e -> status $ mkStatus 500 (error $ show (e :: DatabaseError))


appStatus = do
  v <- lift $ asks (TL.fromStrict . version)
  text ("hello world!\nversion: " <> v)

run :: IO ()
run = do
  conf <- loadCfgFile
  pool <- createDBPool conf
  scottyOptsT def (runIO pool) (routes pool)
