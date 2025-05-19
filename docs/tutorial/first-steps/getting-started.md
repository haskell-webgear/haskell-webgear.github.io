# Getting Started
WebGear is a collection of Haskell packages and can be added to your Haskell project just like any other package.

This version of WebGear supports GHC versions from `9.4` till `9.12`. WebGear supports both
[Cabal](https://cabal.readthedocs.io/) and [Stack](https://haskellstack.org/). The rest of this guide contains
instructions only for Cabal, but they can be adjusted easily for Stack.

## First Project
Let's create the customary hello world project using WebGear. First, create a new Haskell project with Cabal:

```shell-session
$ mkdir webgear-tutorial
$ cd webgear-tutorial
$ cabal init --minimal --non-interactive --exe
```

We'll need a number of language extensions and package dependencies in the cabal file. Edit `webgear-tutorial.cabal` and
add them:

```
default-extensions:
    Arrows
    DataKinds
    DerivingStrategies
    FlexibleContexts
    GeneralizedNewtypeDeriving
    OverloadedStrings
    QuasiQuotes
    ScopedTypeVariables
    TypeApplications
    TypeOperators

build-depends:
    base,
    http-api-data ==0.6.2,
    http-types ==0.12.4,
    openapi3 ==3.2.4,
    time,
    wai ==3.2.4,
    warp ==3.4.7,
    webgear-core ==1.4.0,
    webgear-server ==1.4.0,
    webgear-openapi ==1.4.0
```


Edit `app/Main.hs` and replace its contents with:

```haskell
module Main where

import WebGear.Core
import WebGear.Server
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 3000 helloApp

helloApp :: Wai.Application
helloApp = toApplication $
  [route| HTTP.GET /hello |] $
    proc request ->
      respondA HTTP.ok200 PlainText -< "Hello, World!" :: Text
```

In the above code, WebGear checks if the request method is `GET` and the request URL is `/hello`. If both these
conditions are met, a request handler is invoked. The handler is implemented as an arrow - hence the use of `proc`
syntax. It ignores the `request` input and responds with a 200 OK response having a `text/plain` content type and a
`Hello, World` body.

!!! tip

    If you are unfamiliar with Haskell arrows, you are strongly recommended to go through the
    [Arrows Primer](../advanced/arrows-primer.md) chapter.

## Trying It Out
You may launch the API server with:

```shell-session
$ cabal run
```

And test it with:

```shell-session
$ curl --dump-header - http://localhost:3000/hello
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Wed, 15 May 2025 05:15:46 GMT
Server: Warp/3.4.7
Content-Type: text/plain

Hello, World!
```

## Summary
Congratulations! You've got a basic WebGear application server working. In the next chapter, you'll learn more about
routing HTTP requests and handlers.
