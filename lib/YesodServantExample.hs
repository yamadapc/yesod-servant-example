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

-- * A servant API

type AppAPI = "items" :> Get '[JSON] Value

appAPIServerMock :: Server AppAPI
appAPIServerMock = return $ toJSON [ object [ "id" .= (1 :: Int)
                                            , "name" .= ("one" :: Text)
                                            ]
                                   , object [ "id" .= (2 :: Int)
                                            , "name" .= ("two" :: Text)
                                            ]
                                   , object [ "id" .= (3 :: Int)
                                            , "name" .= ("three" :: Text)
                                            ]
                                   ]

appAPIProxy :: Proxy AppAPI
appAPIProxy = Proxy

-- * The API state to embed

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

-- * The Yesod Application

data App = App { appAPI :: EmbeddedAPI
               }

mkYesod "App" [parseRoutes|
    / HomeR GET
    /api/v1/ SubsiteR EmbeddedAPI appAPI
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    let itemsApiRoute = SubsiteR (EmbeddedAPIR (["items"], []))
    defaultLayout $ [whamlet|
<h1>Hello there!
<p>
  Try testing our items API at
  <a href=@{itemsApiRoute}>@{itemsApiRoute}
|]

-- * The entry-point

run :: Int -> IO ()
run port = do
    let api = serve appAPIProxy appAPIServerMock
    warp port (App (EmbeddedAPI api))
