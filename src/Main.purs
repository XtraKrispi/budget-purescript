module Main where

import Prelude

import Budget.AppM (runAppM)
import Budget.Component.Router as Router
import Budget.Data.Route (Route, routeCodec)
import Budget.FFI.Config (environment)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)

main :: Effect Unit
main = do
  case runExcept environment of
    Left err -> log "Error retrieving environment, application will not start"
    Right env -> do
      log $ "Environment: " <> show env
      HA.runHalogenAff do
        body <- HA.awaitBody
        initialHash <- liftEffect $ getHash
        let 
          rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
          rootComponent = H.hoist (runAppM env) Router.component

          initialRoute :: Maybe Route
          initialRoute = hush  $ parse routeCodec initialHash

        halogenIO <- runUI rootComponent initialRoute body

        void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new

        pure unit
