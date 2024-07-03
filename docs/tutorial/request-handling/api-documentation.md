# API Documentation
You can generate OpenAPI 3 specifications from your handlers using the `webgear-openapi` package. This chapter describes
how to do that.

!!! note

    There is also the `webgear-swagger` package that generates Swagger 2.0 specification. Most of the instructions in
    this chapter can be tweaked to work with that package as well.

## OpenApiHandler
In the [Handlers and Middlewares](handlers-and-middlewares.md) chapter, we encountered the type `#!hs RequestHandler h
 ts`. We learned that `#hs h` is the underlying arrow of the handler. So far, we used the type `#!hs ServerHandler m` in
 place of `#!hs h`. And now, we are going to use a different arrow - `#!hs OpenApiHandler m` - that can generate OpenAPI
 specifications.
 
 As we were using the polymorphic type for our handlers, we don't need to change them at all. Here is the definition of
 `#!hs currentTimeHandler` we encountered earlier.

```haskell
import Control.Monad.IO.Class
import Data.Time
import qualified Network.Wai.Handler.Warp as Warp
import WebGear.Server (toApplication)

currentTimeHandler ::
  forall h m ts.
  (StdHandler h m, MonadIO m) =>
  RequestHandler h ts
currentTimeHandler =
  [route| HTTP.GET /api/now |] $
    proc request -> do
      now <- arrM getNow -< ()
      respondA HTTP.ok200 PlainText -< show now
      where
        getNow :: () -> m UTCTime
        getNow () = liftIO getCurrentTime

main :: IO ()
main = Warp.run 3000 $ toApplication currentTimeHandler
```

You can generate OpenAPI specification of this API with:

```haskell
import Data.OpenApi (OpenApi)
import WebGear.OpenApi (toOpenApi)

specs :: OpenApi
specs = toOpenApi @IO currentTimeHandler
```

That's it! The same handler can generate both a Wai application and its OpenAPI documentation. All the effort we spent
in expressing APIs using traits and middlewares makes this possible.
