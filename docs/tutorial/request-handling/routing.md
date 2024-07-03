# Request Routing
In the previous chapter, we briefly touched upon request routing. It is time to explore it in more detail.

Routing is how an HTTP request is dispatched to an appropriate request handler. WebGear supports routing based on the
request method and/or the request URL path. Let us look at some examples.

## Basic Routing
You may use the template haskell quasiquoter `#!hs route` to dispatch a request to a handler based on the URL path. For
e.g., `#!hs [route| /api/time |] timeHandler` will dispatch the request to `#!hs timeHandler` if the request path is
`/api/time`.

You may further restrict the routing, by specifying an HTTP method in addition to the path.

```haskell
import qualified Network.HTTP.Types as HTTP
...
[route| HTTP.GET /api/time |] timeHandler
```

The above code routes the request to `#!hs timeHandler` only if both the request method and the path matches.

!!! note

    We haven't implemented `#!hs timeHandler` yet and that is intentional. This chapter focuses on routing and we'll not
    pay much attention to implementing request handlers at this point. For now, you may assume that handlers are arrows
    that takes a request as input and produces a response as output - just like the hello world example from the
    previous chapter.

## Capturing Path Variables
In many cases, you may want to treat some parts of the path as a variable to be captured for processing later in the
handler. For example, the below code can be used to retrieve users based on their id.

```haskell
import WebGear.Core
import WebGear.Server
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

userApp :: Wai.Application
userApp = toApplication $
  [route| HTTP.GET /api/user/userId:Int |] getUser
```

Here, `#!hs userId` is a path variable label that captures an value of type `#!hs Int`. The captured variable value can
be accessed by the `#!hs getUser` handler. We'll see how to do that in the next chapter.

You can use any type for the captured variable, provided it has an instance of `#!hs FromHttpApiData` type class. For
example, you can modify the above code to use a custom `#!hs UserId` type:

```haskell
import Web.HttpApiData (FromHttpApiData)

newtype UserId = UserId Int
  deriving newtype (FromHttpApiData)
      
userApp :: Wai.Application
userApp = toApplication $
  [route| HTTP.GET /api/user/userId:UserId |] getUser
```

## Multiple Endpoints
A typical application will have multiple API endpoints and WebGear supports this using the [`#!hs
ArrowPlus`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Control-Arrow.html#t:ArrowPlus) typeclass.

Here is an example. If you want two APIs, one to retrieve a user and another to retrieve a book, both can be combined
using the `#!hs <+>` operation to form a single application.

```haskell
myApp :: Wai.Application
myApp = toApplication $ userAPI <+> bookAPI
  where
    userAPI = [route| HTTP.GET /api/user/userId:Int |] getUser
    bookAPI = [route| HTTP.GET /api/book/bookId:Int |] getBook
```

WebGear will try to route the request in the specified order - first to the user API and then the book API. The request
will be dispatched to the route that matches first. If none of the routes match, a 404 (not found) response will be
returned.

## Path-prefix Matching
In the example above, both the APIs had something in common; they use the `GET` method and have a path prefix of
`/api`. We had to repeat these on both the handlers and that's not ideal. Fortunately, we do not need to do that.

There is another quasiquoter - `#!hs match` - that captures a prefix of the path. Let us use it to capture the common
parts of the APIs and then the `#!hs route` quasiquoter to capture the rest as shown below.

```haskell
myApp :: Wai.Application
myApp = toApplication $
  [match| HTTP.GET /api |] $ userAPI <+> bookAPI
  where
    userAPI = [route| /user/userId:Int |] getUser
    bookAPI = [route| /book/bookId:Int |] getBook
```

## Eliminate the Quasiquoter
While the quasiquoters are very convenient, not everyone likes them. They feel somewhat magicical and require a language
extension. The good news is that you do not have to use them if you don't want to. The `#!hs route` and `#!hs match`
quasiquoters are implemented using four functions described below. You may use them instead of `#!hs route` and `#!hs
match`.

The first function is `#!hs method`. It inspects the HTTP method of the request and dispatches the request to a
specified handler in case of a match.

```haskell
method HTTP.POST postHandler
```

This invokes `#!hs postHandler` only if the request method is `POST`.

The second function is `#!hs path`. It attempts to match a prefix of the request path and dispatches the request to a handler
in case of a match.

```haskell
path "/api" apiHandler
```

This invokes apiHandler when the request path begins with `/api`.

The third function is `#!hs pathVar`. Use it to capture a segment of the request path as a path variable.

```haskell
pathVar @"userId" @Int userHandler
```

This attempts to parse the next segment of the path as an `#!hs Int` and assign the value to a path variable labelled
`#!hs userId`. If that succeeds, `#!hs userHandler` will be invoked.

Finally, we have `#!hs pathEnd`. This function validates that there are no more segments in the request path and invokes a
handler in that case.

```haskell
pathEnd nextHandler
```

You can chain these functions in any order you want to route the requests. For example:

```haskell
method HTTP.GET $ path "/api/user" $ pathVar @"userId" @Int $ pathEnd userHandler
```

This invokes `#!hs userHandler` when the request method is `GET`, the path is of the format `/api/user/<userId>`, and the
`<userId>` portion of the path can be parsed as an `#!hs Int`.

The `#!hs myApp` API from the previous example can be rewritten as:

```haskell
myApp :: Wai.Application
myApp = toApplication $
  method HTTP.GET $ path "/api" $ userAPI <+> bookAPI
  where
    userAPI = path "/user" $ pathVar @"userId" @Int $ pathEnd getUser
    bookAPI = path "/book" $ pathVar @"bookId" @Int $ pathEnd getBook
```

This version does not use any quasiquoters, but is more verbose. In fact, the quasiquoter generates the above code. It
is up to you to choose the version you prefer.

## Summary
In this chapter, you learned how to route requests to handlers based on the request method and path. You also learned to
implement routing with template haskell quasiquoters and regular functions.

Out-of-the-box, WebGear only supports routing based on request method and path. However, you can easily implement
routing based on any other criteria. This is covered in the [reference guide](../advanced/traits.md) in detail.
