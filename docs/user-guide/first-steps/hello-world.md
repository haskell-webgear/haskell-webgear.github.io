# Hello, World!

## First Application

Let us build our first application with WebGear. Create a new project with three files as shown below.

=== "Main.hs"
    ```haskell
    {-# LANGUAGE Arrows #-}
    {-# LANGUAGE OverloadedStrings #-}
    
    module Main where
    
    import qualified Network.HTTP.Types as HTTP
    import qualified Network.Wai.Handler.Warp as Warp
    import WebGear.Server
    
    main :: IO ()
    main =
      Warp.run 3000 $ toApplication $
        method HTTP.GET $
          path "/api/hello" $
            proc request ->
              respondA HTTP.ok200 PlainText -< "Hello, World!" :: Text
    ```

=== "hello.cabal"
    ```
    cabal-version:      2.4
    name:               hello
    version:            0.1.0.0
    
    executable hello
        main-is:          Main.hs
        build-depends:    base >=4.13.0.0 && <4.19
                        , http-types ==0.12.*
                        , warp ==3.3.*
                        , webgear-core ==1.1.0
                        , webgear-server ==1.1.0
        default-language: Haskell2010
    ```

=== "stack.yaml"
    ```yaml
    resolver: nightly-2023-12-25
    allow-newer: true
    ```

You can omit stack.yaml if you are not using stack.

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
5. The `toApplication` converts its argument to a [WAI application](https://hackage.haskell.org/package/wai). This
   application is run as a [Warp](https://hackage.haskell.org/package/warp) server using `Warp.run`.

We will see more details about all these in the upcoming chapters.
