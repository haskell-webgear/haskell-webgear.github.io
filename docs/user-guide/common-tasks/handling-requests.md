# Handling Requests

We learned about the core architecture components in the previous chapters. Now, let us use them to extract information
from requests.

## Query Parameters

If you expect a query parameter in your handler, use the `queryParam` middleware:

```haskell
myHandler ::
  ( StdHandler h m
  , Get h (RequiredQueryParam "limit" Integer) Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body PlainText String] Response
  ) =>
  RequestHandler h ts
myHandler =
  queryParam @"limit" @Integer errorHandler $
    proc request -> do
      let limit :: Integer
          limit = pick @(RequiredQueryParam "limit" Integer) $ from request
      ....

errorHandler ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body PlainText String] Response
  ) =>
  h (Request `With` ts, Either ParamNotFound ParamParseError) Response
errorHandler = proc (_, err) -> respondA HTTP.badRequest400 PlainText -< show err
```

If the query parameter is missing or invalid, the `errorHandler` will be invoked to generate a response. There is also
an `optionalQueryParam` middleware that can be used when the query parameter is not required.

## Header Values

Extracting header values from a request is very similar to query parameters:

```haskell
-- Some type representing the ETag header value
data ETag = ....

-- This instance is used to parse the ETag value
instance FromHttpApiData ETag where
  ....

myHandler ::
  ( StdHandler h m
  , Get h (RequiredRequestHeader "If-Match" ETag) Request
  ) =>
  RequestHandler h ts
myHandler =
  header @"If-Match" @ETag errorHandler $
    proc request -> do
      let etag :: ETag
          etag = pick @(RequiredRequestHeader "If-Match" ETag) $ from request
      ....
```

## Body

You can parse the request body to a Haskell value using the `requestBody` middleware:

```haskell
-- Some type representing a PNG image
data PNGImage = ....

-- This instance is used to parse the body
instance FromByteString PNGImage where
  ....

myHandler ::
  ( StdHandler h m
  , Get h (Body OctetStream PNGImage) Request
  ) =>
  RequestHandler h ts
myHandler =
  requestBody @PNGImage OctetStream errorHandler $
    proc request -> do
      let img :: PNGImage
          img = pick @(Body OctetStream PNGImage) $ from request
      ....
```

Use the `jsonRequestBody` middleware to parse a JSON 
request body:

```haskell
data Person = ....

-- This instance is used to parse the body
instance FromJSON Person where
  ....

myHandler ::
  ( StdHandler h m
  , Get h (Body JSON Person) Request
  ) =>
  RequestHandler h ts
myHandler =
  requestBody @Person JSON errorHandler $
    proc request -> do
      let person :: Person
          person = pick @(Body JSON Person) $ from request
      ....
```

## Conclusion

We covered a few common use cases above, but many others are supported by WebGear. See modules under `WebGear.Core.Trait`
for details.
