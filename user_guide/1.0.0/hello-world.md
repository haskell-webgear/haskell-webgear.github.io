---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Hello, World!

## First Application

Let us build our first application with WebGear. To achieve this goal, you need to create a new project with three files
as shown below.

{% include code-tabs.html files="Main.hs, hello.cabal, stack.yaml" %}

```haskell
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import WebGear.Server

main :: IO ()
main = Warp.run 3000 (toApplication helloHandler)
  where
    helloHandler =
      method HTTP.GET $
        path "/api/hello" $
          proc request -> do
            unlinkA <<< respondA HTTP.ok200 "text/plain" -< "Hello, World!" :: Text
```
{: id="Main-hs-tab-content" class="code-tab-content" }

```
cabal-version:      2.4
name:               hello
version:            0.1.0.0

executable hello
    main-is:          Main.hs
    build-depends:    base >=4.13.0.0 && <4.16
                    , http-types ==0.12.3
                    , warp ==3.3.*
                    , webgear-core ==1.0.1
                    , webgear-server ==1.0.1
    default-language: Haskell2010
```
{: id="hello-cabal-tab-content" class="code-tab-content" }

```yaml
resolver: lts-18.18

extra-deps:
  - webgear-core-1.0.1@sha256:9ed79086eec89fe35ff690d68bf2f0663819fa141009259c727248197fc1e307,4124
  - webgear-server-1.0.1@sha256:e8de1e2699a88c88c9e7ca9909b5a70b7cecd0c8d803a54455db01bf2ce8493e,4991
  - webgear-openapi-1.0.1@sha256:81d5da4ab4b309a4c91c06e78a92415c0534ff75e065e9b1f850824f2316b472,3509
```
{: id="stack-yaml-tab-content" class="code-tab-content" }

## Running the Server

Compile this program with Stack or Cabal and run it. Access the URL <http://localhost:3000/api/hello> and you should be
greeted with a message `Hello, World!`.

## Exploring the code

Here is an explanation of how the above code works:

1. The `method HTTP.GET` function indicates that this API accepts only GET requests.
2. The `path "/api/hello"` function matches a specific request path, any request that does not match this path will
   result in a 404 response.
3. The `proc` notation is special syntax introduced by arrows. You should read the arrows chapter if you are not
   familiar with it.
4. Finally, `respondA` is used to send a `text/plain` response with a body.
5. The `helloHandler` is converted to a [WAI application](https://github.com/yesodweb/wai) with the help of
   `toApplication` and a Warp server is launched.

We will see more details about all these in the upcoming chapters.

{% include prev-next.html %}
