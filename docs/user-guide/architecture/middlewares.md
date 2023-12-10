# Middlewares

Middlewares are higher-order functions that enhance handlers. They accept another handler as a function argument and
produce a handler as the result.

## Request Middlewares

A middleware can enhance a handler by probing for traits in the request.

```haskell
type RequestHandler h req = h (Linked req Request) Response

type Middleware h reqOut reqIn = RequestHandler h reqIn -> RequestHandler h reqOut
```

Here is an example:

```haskell
queryParam ::
  (Get h (RequiredQueryParam name val) Request, ArrowChoice h)
  => h (Linked req Request, Either ParamNotFound ParamParseError) Response  -- ^ error handler
  -> Middleware h req (RequiredQueryParam name val : req)
queryParamHandler errorHandler nextHandler = proc request -> do
  result <- probe QueryParam -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val
```

This middleware chooses between `nextHandler` and `errorHandler` based on the result of probing the `QueryParam`
trait. In this case, `nextHandler` has an additional trait `RequiredQueryParam name val` in its request input.

## Response Middlewares

Middlewares can also enhance responses. Here is an example middleware that sets a response body:

```haskell
setBody ::
  Set h (Body body) Response
  => HTTP.MediaType
  -> h a (Linked ts Response)
  -> h (body, a) (Linked (Body body : ts) Response)
setBody mediaType prevHandler = proc (body, a) -> do
  r <- prevHandler -< a
  plant (Body (Just mediaType)) -< (r, body)
```

The middleware first invokes `prevHandler` and then sets the body on the response produced by it.

## More Use Cases

Middlewares are useful because we can chain a sequence of middlewares to produce a handler as shown in this example:

```haskell
jwtAuth jwk
  $ queryParam @"limit" @Int errorHandler
  $ queryParam @"offset" @Int errorHandler
  $ myHandler
```

Keep in mind that middlewares are not limited to the patterns shown above; there are many other types of transformations
possible. For example, you can have a middleware that captures a [correlation
ID](https://hilton.org.uk/blog/microservices-correlation-id), or generate a new one and inject it as a request
trait. When the underlying handler returns a response, this middleware can add a response header with the correlation
ID.
