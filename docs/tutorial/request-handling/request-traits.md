# Request Traits
In the last chapter, you learned how to extract a path variable from a request using a middleware. WebGear includes many
more traits and middlewares for extracting information from requests. Most of these traits follow a similar
pattern. Their usage looks like this:

```haskell
middleware errorHandler $
  proc request -> do
    let attribute :: TheAttribute = pick @TheTrait $ from request
    ...
```

As you can see, the trait supports a middleware that you wrap your handler with. Some traits support more than one
middleware and you need to select a suitable one.

Most of the middlewares accept an error handler as the first argument. This handler deals with situations when the
middleware encounters an error and needs to return some kind of an error response.

After you wrap the handler with a middleware, you may use the `#!hs pick` function to extract the trait attribute
associated with the request. Note that there are two types involved here. `#!hs TheTrait` is the type of the trait and
`#!hs TheAttribute` is the type of the associated attribute value. In the path variable example from the previous
chapter, these types would be `#!hs PathVar "userId" Int` and `#!hs Int` respectively.

In summary, if you know the suitable middleware, the trait type, and the attribute type, you can extract that attribute
from a request in a type-safe and straightforward manner.

Now that you've seen the general pattern of using request traits, what follows is an in-depth look at the traits and
middlewares included in WebGear.

## Query Parameters
You can extract query parameters from the request and parse the string value into a Haskell type of your choice. 

To illustrate this, consider a variation of the `#!hs currentTimeHandler` from the previous chapter. This time our
handler will accept a query parameter named `#!hs diff` which must be an integer representing a length of time in
seconds. It will then respond with a timestamp calculated by adding this length of time to the current UTC time.

Here is how you could implement this:

```haskell
timeHandler ::
  forall h m ts.
  (StdHandler h m, MonadIO m, Get h (RequiredQueryParam "diff" Integer)) =>
  RequestHandler h ts
timeHandler =
  queryParam @"diff" @Integer errorHandler $
    proc request -> do
      let seconds :: Integer = pick @(RequiredQueryParam "diff" Integer) $ from request
      time <- arrM getTimeDiff -< fromInteger seconds
      respondA HTTP.ok200 PlainText -< show time

errorHandler :: StdHandler h m => h (Request `With` ts, Either ParamNotFound ParamParseError) Response
errorHandler = proc (_, e) ->
  case e of
    Left ParamNotFound ->
      respondA HTTP.badRequest400 PlainText -< "Missing query parameter: diff"
    Right (ParamParseError msg) ->
      respondA HTTP.badRequest400 PlainText -< "Invalid query parameter: diff: " <> msg

getTimeDiff :: MonadIO m => NominalDiffTime -> m UTCTime
getTimeDiff diff = addUTCTime diff <$> liftIO getCurrentTime
```

Let us examine the important parts of this code.

The new constraint `#!hs Get h (RequiredQueryParam "diff" Integer)` is added to indicate that the handler arrow `#!hs h`
must be capable of retrieving the query parameter trait from the request. Then, we can use the `#!hs queryParam @"diff"
@Integer` middleware to ensure that the request contains the parameter. After that, we use `#!hs pick
@(RequiredQueryParam "diff" Integer)` to extract the parameter value as an integer.

Finally, pay close attention to the type of `#!hs errorHandler`. It's input is a pair containing the original request
and an error object. There are two possible types of error - either the parameter is missing or it cannot be parsed as
an integer. These two cases are represented using the type `#!hs Either ParamNotFound ParamParseError`.

WebGear has three more middlewares related to query parameters. They are listed below:

| Middleware                       | Trait Type                                                                       | Attribute Type                 |
|----------------------------------|----------------------------------------------------------------------------------|--------------------------------|
| `#!hs optionalQueryParam`        | `#!hs QueryParam Optional Strict name val` or `#!hs OptionalQueryParam name val` | `#!hs Maybe val`               |
| `#!hs lenientQueryParam`         | `#!hs QueryParam Required Lenient name val`                                      | `#!hs Either Text val`         |
| `#!hs optionalLenientQueryParam` | `#!hs QueryParam Optional Lenient name val`                                      | `#!hs Maybe (Either Text val)` |

Their usage is very similar to `#!hs queryParam`. The `Optional` variants can be used when the query parameter is not
mandatory and the `Lenient` variants can be used when you don't want to fail in case of parsing errors. Note that the
trait and attribute types change accordingly.

## Headers
Request headers can be accessed very similar to query parameters in WebGear. These are the middlewares and associated
types for request headers:

| Middleware                   | Trait Type                                                                             | Attribute Type                 |
|------------------------------|----------------------------------------------------------------------------------------|--------------------------------|
| `#!hs header`                | `#!hs RequestHeader Required Strict name val` or `#!hs RequiredRequestHeader name val` | `#!hs Maybe val`               |
| `#!hs optionalHeader`        | `#!hs RequestHeader Optional Strict name val` or `#!hs OptionalRequestHeader name val` | `#!hs Maybe val`               |
| `#!hs lenientHeader`         | `#!hs RequestHeader Required Lenient name val`                                         | `#!hs Either Text val`         |
| `#!hs optionalLenientHeader` | `#!hs RequestHeader Optional Lenient name val`                                         | `#!hs Maybe (Either Text val)` |

Since they are very similar to query parameters, we'll omit an example here. You may refer to the API documentation for
more details.

## Cookies
There are two middlewares for access request cookies:

| Middleware            | Trait Type                      | Attribute Type   |
|-----------------------|---------------------------------|------------------|
| `#!hs cookie`         | `#!hs Cookie Required name val` | `#!hs val`       |
| `#!hs optionalCookie` | `#!hs Cookie Optional name val` | `#!hs Maybe val` |

As you've probably guessed, the former is used in case of mandatory cookies and the latter in case of optional
cookies.

As an example, the code below accesses a mandatory cookie named `session-id` from the request.

```haskell
sessionHandler :: (StdHandler h m, Get h (Cookie Required "session-id" Text)) => RequestHandler h ts
sessionHandler =
  cookie @"session-id" @Text errorHandler $
    proc request -> do
      let sessionId = pick @(Cookie Required "session-id" Text) $ from request
      respondA HTTP.ok200 PlainText -< "Session ID: " <> sessionId

```

But this raises a compilation error:

```
• Could not deduce ‘HasTrait
                       (RequestHeader Required Strict "Cookie" Text) ts’
    arising from a use of ‘cookie’
  from the context: (StdHandler h m,
                     Get h (Cookie Required "session-id" Text))
```

This is because extracting a cookie is a two step process. First, you need to get the `Cookie` header from the request
and then extract a cookie from the header value. WebGear models such dependencies using trait prerequisites. In this
case, the `#!hs RequestHeader Required Strict "Cookie" Text` trait is a prerequisite for the `#!hs Cookie Required
"session-id" Text` trait.

To satisfy the prerequisite, we make a couple of changes:

```haskell
sessionHandler ::
  ( StdHandler h m
  , Gets h [RequiredRequestHeader "Cookie" Text, Cookie Required "session-id" Text]
  ) => RequestHandler h ts
sessionHandler =
  header @"Cookie" @Text errorHandler $
    cookie @"session-id" @Text errorHandler $
      proc request -> do
        let sessionId = pick @(Cookie Required "session-id" Text) $ from request
        respondA HTTP.ok200 PlainText -< "Session ID: " <> sessionId
```

The `#!hs header @"Cookie" @Text` middleware will satisfy the prerequisite. But you also need to add a `#!hs Get`
constraint for this new trait to the type signature. In this case, we used the `#!hs Gets` constraint which supports a
list of traits. This is a concise form as compared to adding one constraint for each trait.

## Request Body
The `#!hs requestBody` middleware is used to extract the HTTP body from a request as a Haskell value. It accepts two
arguments - a MIME type that determines how the body is converted to the Haskell value and an error handler for
processing errors.

As an example, let us look at how to parse a JSON formatted request body into a Haskell type:

```haskell
data MyRecord = MyRecord
  { -- some JSON fields go here
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

jsonBodyHandler :: (StdHandler h m, Get h (Body JSON MyRecord)) => RequestHandler h ts
jsonBodyHandler =
  requestBody @MyRecord JSON errorHandler $
    proc request -> do
      let record :: MyRecord = pick @(Body JSON MyRecord) $ from request
      ....
```

WebGear includes more MIME types apart from `#!hs JSON`. These are defined in the `#!hs WebGear.Core.MIMETypes` module:

* `#!hs HTML` - The `text/html` MIME type
* `#!hs PlainText` - The `text/plain` MIME type
* `#!hs OctetStream` - The `application/octet-stream` MIME type
* `#!hs FormURLEncoded` - The `application/x-www-form-urlencoded` MIME type
* `#!hs FormData` - The `multipart/form-data` MIME type

Parsing the HTTP body to a Haskell value is defined through the `#!hs BodyUnrender` type class defined in the `#!hs
WebGear.Server.MIMETypes` module. Instances of this type class includes the logic to parse the body according to the
rules of the MIME type.

## Summary
In this chapter, you learned about the commonly used traits and middlewares to extract information from requests. You
may find more information about these in their API documentation.

The traits and middlewares included in WebGear are sufficient for most common tasks. If you encounter a use case which
is not covered by these traits, it is possible to implement your own traits to handle such cases. We will explore this
in a [subsequent chapter](../advanced/traits.md).
