Yesod & Servant Embedding Example
=================================
This is an example of how you can embed a
[Servant API](https://github.com/haskell-servant/servant/)
within an
[Yesod](http://www.yesodweb.com) application.

It's currently live at [yesod-servant-example.herokuapp.com/](https://yesod-servant-example.herokuapp.com/)

I'm sure you can do the opposite, embedding an Yesod application onto a servant
API, and it might even be easier, but neither documentations on the matter are
very easy for me personally and Yesod had an example cited on these articles:

- [Creating a subsite](http://www.yesodweb.com/book/creating-a-subsite)
- [Announcing Yesod 0.7](http://www.yesodweb.com/blog/2011/02/announcing-yesod-0-7)
    - Which in turn pointed to:
        - [yesod-static](https://hackage.haskell.org/package/yesod-static-1.5.0.3/docs/src/Yesod-EmbeddedStatic-Internal.html#EmbeddedStatic)

Some language extensions
------------------------

I've been trying to not use `LANGUAGE` pragmas inline, in the same way one does
not keep their front-end asset pipeline configuration inline, through using the
`default-extensions` field of `hpack` / `cabal` manifests.

Since this is an example, here are the extensions required for this to work,
with a link to some documentation in case you are unfamiliar.

Just skip to the next heading in case this is boring.

> {-# LANGUAGE DataKinds             #-}

`DataKinds` is required for the `"item" :> Get '[JSON] Value` type-level DSL
for servant. As far as I understand, it just lets us use value types at the
type-level.

- [GHC Documentation](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html)
- [Also good discussion on "Opalaye's sugar on top" first section](http://ren.zone/articles/opaleye-sot)

> {-# LANGUAGE FlexibleInstances     #-}

`FlexibleInstances` is one of the extensions Emacs adds for me whenever I'm
writing type-class based Haskell code. It allows for `instance Read a => Stuff
a` type of type-class instance declarations.

- [Brief blog post "My First Introduction to Haskell Extensions: FlexibleInstances"](http://connectionrequired.com/blog/2009/07/my-first-introduction-to-haskell-extensions-flexibleinstances/)
- [Haskell Prime Documentation](https://prime.haskell.org/wiki/FlexibleInstances)
- [Succinct Stack Overflow Answer](http://stackoverflow.com/questions/20145943/flexible-instances-needed)

> {-# LANGUAGE MultiParamTypeClasses #-}

`MultiParamTypeClasses` allows multi-param type-classes.

- [24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html)

> {-# LANGUAGE OverloadedStrings     #-}

`OverloadedStrings` overloads `"stuff"` syntax with whatever types define
[`Data.String.IsString`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-String.html#t:IsString)
instances, such as `Text`, `ByteString` or whatever.

- [School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings)
- [24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html)
- [`Data.String.IsString`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-String.html#t:IsString)

_Extra-credit:_ there's also a neat extension `OverloadedLists` which does a
similar thing for `[a, b, c]` syntax.

- [Haskell Wiki](https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists)
- [`GHC.Exts.IsList`](http://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-Exts.html#t:IsList)

> {-# LANGUAGE PackageImports        #-}

`PackageImports` allow `import "package-name" Module.Name`, I'm using it so you
know where things come from.

- [School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#packageimports)

> {-# LANGUAGE QuasiQuotes           #-}
> {-# LANGUAGE TemplateHaskell       #-}

`QuasiQuotes` allow for `[stuff| random custom syntax |]` where `stuff` will be
a
[`Language.Haskell.TH.Quote.QuasiQuoter`](http://hackage.haskell.org/package/template-haskell-2.11.0.0/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter). Essentially
`stuff` is something that takes a string and returns some
declaration/expression/pattern/etc AST.

`TemplateHaskell` allows for template haskell, which is how you take ASTs and
paste onto the code.

- [24 Days of GHC Extensions on Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)

> {-# LANGUAGE RecordWildCards       #-}

`RecordWildCards` let us open a record when pattern matching, binding all its
fields to local variables.

- [24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html)

> {-# LANGUAGE TypeFamilies          #-}

`TypeFamilies` is required for type-classes that have associated types on the
instances, like `persistent` `Key User` type of things.

- [24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html)

> {-# LANGUAGE TypeOperators         #-}

`TypeOperators` allows infix operators at the type-level.

- [24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html)

The module
------------------------

We import some stuff from a couple of packages, thankfully not that many imports.

> module YesodServantExample where
> import           "text" Data.Text        (Text)
> import           "wai" Network.Wai
> import           "servant-server" Servant          hiding (Handler)
> import           "yesod" Yesod
> import           "yesod-core" Yesod.Core.Types

The Servant bits
------------------------

Here's the type for a servant API that should read as, for the route "items",
accept `GET` requests with `Content-Type: application/json` responding with
[`Data.Aeson.Value`](http://hackage.haskell.org/package/aeson-1.0.0.0/docs/Data-Aeson.html#t:Value)s

> type AppAPI = "items" :> Get '[JSON] Value

This is the implementation for the API. Since there's only one route, there's
no need to break this into multiple expressions.

We use helpers for constructing `Value`s from `aeson`.

`Servant.Server.Server` is at
[http://hackage.haskell.org/package/servant-server-0.7.1/docs/Servant-Server.html](http://hackage.haskell.org/package/servant-server-0.7.1/docs/Servant-Server.html).

> appAPIServerMock :: Server AppAPI
> appAPIServerMock = return $ toJSON [ object [ "id" .= (1 :: Int)
>                                             , "name" .= ("one" :: Text)
>                                             ]
>                                    , object [ "id" .= (2 :: Int)
>                                             , "name" .= ("two" :: Text)
>                                             ]
>                                    , object [ "id" .= (3 :: Int)
>                                             , "name" .= ("three" :: Text)
>                                             ]
>                                    ]

The way I understand it, `Proxy` is a way to demote types to the value
level. It's "documented"
[here](http://hackage.haskell.org/package/servant-server-0.7.1/docs/Servant.html#t:Proxy).

> appAPIProxy :: Proxy AppAPI
> appAPIProxy = Proxy

The Servant Yesod Subsite bits
------------------------

To embed the servant API as an `yesod` subsite, we need a data-type for the
state to embed. This is just a mock, so I'm just wrapping the
[`Network.Wai.Application`](https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html#t:Application)
we'll create from our `appAPIServerMock`

> data EmbeddedAPI = EmbeddedAPI { eapiApplication :: Application
>                                }

Apparently there needs to be some Yesod subsite boilerplate, though this could
be pushed to a package (maybe on this repository even! :) ).

I don't care about type-safe URLs for the example, because it sounds like doing
this bridge is going to be some work, so I'm just wrapping what seems to be
[`Network.HTTP.Types.URI.decodePath`](https://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-URI.html#v:decodePath)
output. A tuple of the path pieces (each `/` separated bit) and a map of the
querystring values.

This declares a `EmbeddedAPIR ([Text], [(Text, Text)])` type for paths to the
embedded API. So anywhere on the `Yesod` source-code we can reference it in a
"type-safe" manner.

> instance RenderRoute EmbeddedAPI where
>   data Route EmbeddedAPI = EmbeddedAPIR ([Text], [(Text, Text)])
>     deriving(Eq, Show, Read)
>   renderRoute (EmbeddedAPIR t) = t
>
> instance ParseRoute EmbeddedAPI where
>   parseRoute t = Just (EmbeddedAPIR t)

We also seem to need this thing. I just copied my way through `yesod-static`
and friends, since I couldn't find documentation.

(Well, there's a chapter in the book. But I don't want to read a book this
morning. As constructive criticism of this, [here's `Express.js` documentation
on the same functionality at 6 lines and with 2 examples.](http://expressjs.com/en/4x/api.html#router))

Pretty-much, what it seems we're doing is reading the `EmbeddedAPI` state from
an arbitrary Yesod instance and passing-through the request to it. Again, this
could be in a package.

> instance Yesod master => YesodSubDispatch EmbeddedAPI (HandlerT master IO) where
>   yesodSubDispatch YesodSubRunnerEnv{..} req = resp
>     where
>       master = yreSite ysreParentEnv
>       site = ysreGetSub master
>       resp = eapiApplication site req

The Yesod Application
------------------------

Now comes the standard Yesod stuff, which has plenty of documentation. We
define a type for our Application state, which in this case will just contain
our `EmbeddedAPI` API state.

> data App = App { appAPI :: EmbeddedAPI
>                }

And we call `mkYesod` to generate the code for our route definitions

This defines a `HomeR` constructor (with no arguments), that represents a route
that accepts GET requests, automatically bound to the `getHomeR` function. It also defines a
`SubsiteR (EmbeddedAPIR ([Text], [(Text, Text)]))` constructor for our API
routes, which was what we defined above. With some magic maybe you could bridge
servant type definitions with this style of `TypeR` route representations.

> mkYesod "App" [parseRoutes|
>     / HomeR GET
>     /api/v1/ SubsiteR EmbeddedAPI appAPI
> |]

There's an empty
[`Yesod`](https://hackage.haskell.org/package/yesod-core-1.4.20.1/docs/Yesod-Core.html)
instance to make the compiler happy. Usually you'd define things like how
`defaultLayout` behaves and so on here.

> instance Yesod App

Our `getHomeR` handler responds with some `hamlet` Widget, which demonstrates
how you could use that `EmbeddedAPIR` constructors.

> getHomeR :: Handler Html
> getHomeR = do
>     let itemsApiRoute = SubsiteR (EmbeddedAPIR (["items"], []))
>     defaultLayout $ [whamlet|
> <h1>Hello there!
> <p>
>   Try testing our items API at
>   <a href=@{itemsApiRoute}>@{itemsApiRoute}
> |]

The entry-point
------------------------

And finally comes the entry-point, which just takes the port and uses:

- [`Servant.Server.serve`](http://hackage.haskell.org/package/servant-server-0.7.1/docs/Servant-Server.html)
  to convert the servant `Server` onto an wai
  `Application`
- `warp` to convert Yesod instances to wai Applications and run them with warp

`main` would just be `YesodServantExample.run 3000`. It's in the repository
too.

> run :: Int -> IO ()
> run port = do
>     let api = serve appAPIProxy appAPIServerMock
>     warp port (App (EmbeddedAPI api))
