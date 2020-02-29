{-# LANGUAGE OverloadedStrings #-}

module SpecsIT where

import Data.Aeson (encode)
import Test.Hspec
import Test.Hspec.Wai
import Text.Printf
import Network.Wai                    ( Application )
import Web.Scotty.Trans (scottyAppT)
import Control.Monad.Reader           ( runReaderT)
import Data.Default.Class             ( def )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Lib as L
import Models.DTO
import Data.Types

app :: IO Application
app = scottyAppT runIO L.routes where runIO m = runReaderT (rApp m) def

helloSpec =
  with app $
  describe "GET /hello" $ do
    it "responds with 200" $ 
      get "/hello" `shouldRespondWith` 200
    it "responds with 404" $ 
      get "/s/notfound" `shouldRespondWith` 404
    it "responds with version" $ 
      get "/hello" `contains` "version"

postUserSpec =
  with app $
  describe "POST /user" $
  it "responds with 200" $ 
    postUser `shouldRespondWith` 200
  where
    postUser = post "/user" (encode $ UserDTO "gigi@gigi.ro" "password")

getUserSpec =
  with app $
  describe "GET /user/gigi@gigi.ro" $
  it "responds with 200" $
    getUser `shouldRespondWith` 200
  where
    getUser = get "/user/gigi@gigi.ro"

contains = (. containMatcher) . shouldRespondWith

containMatcher :: BS.ByteString -> ResponseMatcher
containMatcher val = ResponseMatcher 200 [] (MatchBody matcher)
  where
    matcher _ body =
      if BS.isInfixOf val (toStrict body) 
         then Nothing
         else Just $ printf "%s doesn't contain %s" (show body) (show val)
