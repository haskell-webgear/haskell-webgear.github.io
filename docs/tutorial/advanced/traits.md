# Trait Internals
The previous section of the tutorial showcased many traits and middlewares included in WebGear. Occasionally, you may
want to extend these traits or implement your own traits. This chapter reveals all the internals of traits to help you
do this.

## Basics
The traits included in `webgear-core` package are not special in anyway; you may define your own traits that are as
first-class as the traits provided by WebGear. We'll use an example to demonstrate this.

Many systems use a special HTTP header `X-Request-ID` for distributed tracing. This header value is often included in
logs and telemetry to trace the progress of a request through all the systems involved in processing it. 

## Implementation
Let us see how to implement a trait to capture the request ID value from HTTP requests.

### 1. Define a Trait Type
The first step in defining any trait is to define a type representing the trait. Here is the type for our request ID
trait:

```haskell
data RequestID = RequestID
```

We don't need anything fancy here because our trait is very simple. If you need any additional configuration associated
with your trait, you may let the data constructor accept those as parameters.

### 2. Define a Trait Attribute
Next we define a type for the attribute value associated with our trait type. This is the value that we get when we
extract the request ID successfully from the request.

```haskell
newtype RequestIDValue = RequestIDValue Text

type instance Attribute RequestID Request = RequestIDValue
```

We use a newtype wrapper around `#!hs Text` as we don't assume any further structure to the request ID header.

### 3. Define Trait Absence
Next we need to handle the case of missing trait attributes. What if the request does not contain a request ID? We'll
indicate that as an error.

```haskell
data RequestIDMissing = RequestIDMissing

type instance Absence RequestID = RequestIDMissing
```

### 4. Define Prerequisites
Optionally, a trait can have prerequisites. These are usually other traits that you want to depend on. In this case,
we'll use `#!hs OptionalRequestHeader` as a prerequisite because the request ID is derived from a request header.

```haskell
type RequestIDHeader = RequestHeader Optional Lenient "X-Request-ID" Text

type instance Prerequisite RequestID ts = HasTrait RequestIDHeader ts
```

You may set this type instance to `#!hs ()` if the trait does not have any prerequisites.

### 5. Implement Get Instances
Now we can implement instances of `#!hs Get` type class for each type of handler arrows that we want to support.

```haskell
instance Monad m => Get (ServerHandler m) RequestID where
  getTrait ::
    HasTrait RequestIDHeader ts =>
    RequestID ->
    ServerHandler m (Request `With` ts) (Either RequestIDMissing RequestIDValue)
  getTrait RequestID = proc request -> do
    let hdr = pick @RequestIDHeader $ from request
    case hdr of
      Just (Right val) -> returnA -< Right (RequestIDValue val)
      Just (Left _) -> returnA -< Left RequestIDMissing
      Nothing -> returnA -< Left RequestIDMissing
```

Notice how the implementation uses the prerequisite to get the header value. The `#!hs getTrait` method returns an `#!hs
Either` value - the `#!hs Left` value must be of the `#!hs Absence` type and the `#!hs Right` value must be of the `#!hs
Attribute` type.

You can also define additional `#!hs Get` type class instances - for example, define an `#!hs OpenApiHandler` instance
if you want to include this trait in the OpenAPI documentation.

### 6. Implement a Middleware
It is a good practice to define one or more middlewares for your trait. They make it convenient to use the trait in API
handlers. Here is a middleware for the request ID trait.

```haskell
requestID ::
  (Get h RequestID, ArrowChoice h, HasTrait RequestIDHeader ts) =>
  h (Request `With` ts, RequestIDMissing) Response ->
  Middleware h ts (RequestID : ts)
requestID errorHandler nextHandler =
  proc request -> do
    result <- probe RequestID -< request
    case result of
      Left e -> errorHandler -< (request, e)
      Right request' -> nextHandler -< request'
```

The `#!hs probe` arrow makes use of the `#!hs getTrait` implementation above. If `#!hs getTrait` fails with an error,
probe too will fail with a `#!hs Left` value. But if `#!hs getTrait` succeeds, `#!hs probe` will return a request value
with the additional `#!hs RequestID` trait attached to it. In other words, the type of `#!hs request'` in the above
middleware is ``#!hs Request `With` (RequestID : ts)``.

As with other handlers we've seen so far, this middleware is also polymorphic in the type variable `#!hs h`. It will use
the appropriate handler arrow as required - `#!hs ServerHandler`, `#!hs OpenApiHandler`, etc.
