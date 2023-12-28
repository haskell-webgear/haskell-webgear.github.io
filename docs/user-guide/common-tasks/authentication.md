# Authentication

WebGear has first class support for common authentication schemes and mechanisms to implement any custom authentication
schemes you need. Let us explore these.

## Basic Authentication

Typical usage of basic authentication is:

```haskell
{-
 * The credentials are validated in the IO monad.
 * A `Username' is extracted on success and a `Text' error message on failure.
-}
type UserAuth = BasicAuth IO Text Username

helloHandler ::
  ( StdHandler h IO,
  , Get h UserAuth Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body PlainText String] Response
  ) =>
  RequestHandler h '[]
helloHandler =
  basicAuth authConfig authFail $
    proc request -> do
      let username = pick @UserAuth $ from request
          msg = "Hello, " <> show username <> "!"
      respondA HTTP.ok200 PlainText -< msg

-- Send a response when authentication fails
authFail ::
  ( StdHandler h IO
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body PlainText String] Response
  ) =>
  h (Request `With` ts, BasicAuthError Text) Response
authFail = proc (_request, err) ->
  respondA HTTP.forbidden403 PlainText -< show err

-- This is where you validate the credentials
authConfig :: UserAuth
authConfig = BasicAuth'{ .... }
```

The `authFail` handler comes into play when authentication fails. That is, either credentials are missing in the
request, or cannot be formatted, or fails the validation specified in `authConfig`.

Basic authentication support is not limited to this. There are more middlewares in the `WebGear.Core.Trait.Auth.Basic`
module that allows custom authentication schemes instead of the standard `Basic` scheme. They also support optional
authentication where the credentials may not always be present in the request.

## JWT Authentication

JWT authentication with a bearer token is similar to basic authentication:

```haskell
type UserAuth = JWTAuth IO Text Username

helloHandler ::
  ( StdHandler h IO
  , Get h UserAuth Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body PlainText String] Response
  ) =>
  RequestHandler h '[]
helloHandler =
  jwtAuth authConfig authFail $
    proc request -> do
      let username = pick @UserAuth $ from request
          msg = "Hello, " <> show username <> "!"
      respondA HTTP.ok200 PlainText -< msg

-- Configuration to validate JWT
authConfig :: UserAuth
authConfig =
  JWTAuth'
    { jwtValidationSettings = ....
    , jwkSet = ....
    , toJWTAttribute = ....
    }
```

Similar to basic authentication, more middlewares are available in the `WebGear.Core.Trait.Auth.JWT` module.

## Custom Authentication

WebGear supports custom authentication requirements if your application needs it. Here is how you build it:

1. Define a trait `t` for the authentication scheme. Implement `Trait` and `TraitAbsence` instances for this trait.
2. Implement a `Get h t Request` typeclass instance for every handler `h` that you intend to support.
3. Implement middlewares that `probe` for the newly defined trait.

That's all! The middlewares are now ready for use in your handlers.
