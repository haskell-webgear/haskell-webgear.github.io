# Generating Responses
All handlers must respond with a valid HTTP response. This chapter discusses different ways to create and return a
response from handlers.

## Basic Responses
The easiest way to return a response is to use the `respondA` arrow. You've already seen it in use a few times:

```haskell
import qualified Network.HTTP.Types as HTTP
...
respondA HTTP.ok200 PlainText -< "Hello, WebGear"
```

The first argument to `respondA` is the HTTP status code and the second argument is the MIME type of the response.  The
body is passed as the input to the arrow. Needless to say, the type of the body must be compatible with the MIME
type. For e.g., the `#!hs JSON` MIME type requires that the type of the body has an instance of the `#!hs ToJSON` type
class from the aeson package.

In general, the MIME type and the body type are connected via the `#!hs WebGear.Server.MIMETypes.BodyRender` type
class. An instance of this type class must exist to use that body type with the MIME type.

## Advanced Responses
While `#!hs respondA` is useful, there are cases where you might need lower level access to the response
generation. Fortunately, that's very easy with WebGear.

You can generate an empty response with just a status code using `#!hs WebGear.Core.Trait.Status.mkResponse`. It works
like this:

```haskell
proc x -> do
  response <- mkResponse HTTP.ok200 -< ()
  unwitnessA -< response
```

The `#!hs unwitnessA` arrow converts a value of type ``#!hs Response `With` ts`` to a value of type `#!hs Response`. In
other words, it removes all the trait evidences associated with the response. This must be done before the handler
returns the response to the caller.

The `#!hs WebGear.Core.Trait.Status` module contains convenience functions to generate responses for all common status
codes.

You can specify the response body either using `#!hs WebGear.Core.Trait.Body.setBody` or  `#!hs
WebGear.Core.Trait.Body.setBodyWithoutContentType`. As the name indicates, the latter does not require you to know the
MIME type in advance.

```haskell
proc x -> do
  response <- mkResponse HTTP.ok200 -< ()
  let body = MyJSONRecord{...}
  response' <- setBody JSON -< (response, body)
  unwitnessA -< response
```

You can specify response headers using `#!hs WebGear.Core.Trait.Header.setHeader` arrow:

```haskell
proc x -> do
  response <- mkResponse HTTP.ok200 -< ()
  response' <- setHeader @"Cache-Control" @Text -< (response, "max-age=180")
  unwitnessA -< response
```

All these arrows assume that you know the response traits statically. In the above examples, you knew the HTTP status
code, the body MIME type, and the header name without having to invoke the handler with a request. On the other hand,
the response body value and the header value were passed as inputs to these arrows; i.e they are dynamically computed
when the handler is *executed*.

This separation of static an dynamic parts of an API allows WebGear to extract the static information without having to
*execute* the handler. By examining the handler, WebGear deduces that it returns an HTTP response with status code 200,
having a JSON body, and a `Cache-Control` header. This is useful in many ways; for e.g. this allows us to generate
documentation or generate clients automatically.

What if you truly do not know static information about some traits? What if you need to dynamically compute them? You
can definitely do that. The `#!hs WebGear.Core.Response` module shows how you can construct a response without using any
of the trait machinery. The `#!hs Response` type has data constructors that are very similar to the `#!hs Response` type
from Wai. 

For example:

```haskell
proc x -> do
  status <- arrM getResponseStatus -< ()
  headers <- arrM getResponseHeaders -< ()
  body <- arrM getResponseBody -< ()
  returnA -< Response status headers body
```

This response is entirely computed at runtime and you don't use the trait mechanism at all. However, there is a
trade-off: WebGear does not know any static information about it. As a consequence, you lose the ability to generate
documentation from this handler.

You may also choose to dynamically generate some parts of the response while using traits for the rest. This will retain
some static information about the handlers that might come handy.

## MIME Types
MIME types are used to convert a request body to a Haskell value or to generate a response body from a Hakell
value. Commonly used MIME types are defined in `#!hs WebGear.Core.MIMETypes` module; but you may define your own MIME
types if required.

The first step to add a new MIME type is to define a Haskell type for it and provide an instance of the `#!hs
WebGear.Core.MIMETypes.MIMEType` type class. This type class has a single method - `#!hs mimeType` - to determine the
media type associated with the MIME type.

Next, define a couple more type class instances from the `#!hs WebGear.Server.MIMETypes` module.

The `#!hs BodyRender` type class is used to serialize a Haskell value to a response body. The `#!hs BodyUnrender` type
class works parses the request body into a Haskell value. Defining instances for these two classes allows you to use
your MIME type in request/response handling.

## Summary
Response generation is very straightforward in WebGear. At the same time, you have the flexibility to bypass WebGear's
trait machinery should the need arise.
