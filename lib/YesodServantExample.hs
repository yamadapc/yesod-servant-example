{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module YesodServantExample where

import           "text" Data.Text        (Text)
import           "wai" Network.Wai
import           "servant-server" Servant          hiding (Handler)
import           "yesod" Yesod
import           "yesod-core" Yesod.Core.Types

data EmbeddedAPI = EmbeddedAPI { eapiApplication :: Application
                               }

instance RenderRoute EmbeddedAPI where
  data Route EmbeddedAPI = EmbeddedAPIR ([Text], [(Text, Text)])
    deriving(Eq, Show, Read)
  renderRoute (EmbeddedAPIR t) = t

instance ParseRoute EmbeddedAPI where
  parseRoute t = Just (EmbeddedAPIR t)

instance Yesod master => YesodSubDispatch EmbeddedAPI (HandlerT master IO) where
  yesodSubDispatch YesodSubRunnerEnv{..} req = resp
    where
      master = yreSite ysreParentEnv
      site = ysreGetSub master
      resp = eapiApplication site req

data App = App { appAPI :: EmbeddedAPI
               }

mkYesod "App" [parseRoutes|
    / HomeR GET
    /api/v1/ SubsiteR EmbeddedAPI appAPI
|]

instance Yesod App

type AppAPI = "items" :> Get '[JSON] Text

appAPIServer :: Server AppAPI
appAPIServer = return "Hello there"

getHomeR :: Handler Html
getHomeR = undefined

appAPIProxy :: Proxy AppAPI
appAPIProxy = Proxy

run :: Int -> IO ()
run port = do
    let api = serve appAPIProxy appAPIServer
    warp port (App (EmbeddedAPI api))
