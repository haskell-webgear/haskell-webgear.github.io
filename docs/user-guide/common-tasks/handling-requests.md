# Handling Requests

We learned about the core architecture components in the previous chapters. Now, let us use them to extract information
from requests.

## Query Parameters

If you expect a query parameter in your handler, use the `queryParam` middleware:

```haskell
myHandler ::
  StdHandler h m
    '[RequiredQueryParam "limit" Integer]
    [RequiredHeader "Content-Type" Text, Body String] =>
  RequestHandler h req
myHandler =
  queryParam @"limit" @Integer errorHandler $
    proc request -> do
      let limit :: Integer
          limit = pick @(RequiredQueryParam "limit" Integer) $ from request
      ....

errorHandler ::
  StdHandler h m '[] [RequiredHeader "Content-Type" Text, Body String] =>
  h (Linked req Request, Either ParamNotFound ParamParseError) Response
errorHandler = proc (_, err) -> unlinkA <<< respondA HTTP.badRequest400 "text/plain" -< show err
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
  StdHandler h m '[RequiredHeader "If-Match" ETag] '[] =>
  RequestHandler h req
myHandler =
  header @"If-Match" @ETag errorHandler $
    proc request -> do
      let etag :: ETag
          etag = pick @(RequiredHeader "If-Match" ETag) $ from request
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
  StdHandler h m '[Body PNGImage] '[] =>
  RequestHandler h req
myHandler =
  requestBody @PNGImage (Just "image/png") errorHandler $
    proc request -> do
      let img :: PNGImage
          img = pick @(Body PNGImage) $ from request
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
  StdHandler h m '[JSONBody Person] '[] =>
  RequestHandler h req
myHandler =
  jsonRequestBody @Person errorHandler $
    proc request -> do
      let person :: Person
          person = pick @(JSONBody Person) $ from request
      ....
```

## Conclusion

We covered a few common use cases above, but many others are supported by WebGear. See modules under `WebGear.Core.Trait`
for details.
