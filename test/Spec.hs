{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State (execState)
import Data.Default.Class (def)
import Test.Hspec

import qualified Lib as L
import qualified Web.Scotty.Internal.Types as ST

import SpecsIT

main :: IO ()
main = do
  mapM_ hspec unitSpecs
  mapM_ hspec itSpecs

routeSpec :: Spec
routeSpec = describe "Lib" $ it "has 3 routes" $ do
  let allRoutes = ST.routes (execState (ST.runS L.routes) def)
  length allRoutes `shouldBe` (5 :: Int)

unitSpecs :: [Spec]
unitSpecs = [routeSpec]

itSpecs :: [Spec]
itSpecs = [helloSpec, postUserSpec]



