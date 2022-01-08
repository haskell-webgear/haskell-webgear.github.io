---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Handlers

This chapter explains the architecture of WebGear handlers.

## Handler Functions

Handlers are components that accept a request as input and produce a response as output. A good first intuition is that
they can be represented as functions of type `Monad m => Request -> m Response`. We compute the response in a monad
because it typically has some side effects, such as accessing a database, logging etc.

It is useful to represent handlers as functions because you can split the logic to as many functions as you like and
compose them to form a handler. The ability to compose small, manageable pieces of code is very important to build
maintainable applications.

For example, let us consider a handler that needs to:

1. Extract a query parameter from the request.
2. Retrieve some data from a database using this parameter.
3. Form a response based on the retrieved data.

If there are three functions corresponding to these steps:

```haskell
extractQueryParam :: Moand m => Request -> m QueryParam
extractQueryParam = ....

retrieveDataFromDB :: Monad m => QueryParam -> m DBData
retrieveDataFromDB = ....

makeResponse :: Monad m => DBData -> m Response
makeResponse = ....
```

Now you can compose these to form a handler:

```haskell
handler :: Monad m => Request -> m Response
handler = extractQueryParam >=> retrieveDataFromDB >=> makeResponse
```

However, there is one drawback to this representation. The only thing you can do with this function is to evaluate it by
supplying a request. What if we want to get the name of the query parameter used or the HTTP status code of the response
*without* running/evaluating the handler? This kind of information is useful to generate documentation about the
handler, automatically build a client program for the API etc. Unfortunately, the monadic function representation is not
suited for anything like that.

## Handler Arrows

The problem we face is that we need to maintain some static information about the handler that is independent of the
function evaluation. WebGear uses arrows[^1] to solve this problem. They support some static parts in addition to the
dynamic evaluation part.

Let us look at the hello world handler from the previous chapter again:

```haskell
proc request ->
  unlinkA <<< respondA HTTP.ok200 "text/plain" -< "Hello, World!" :: Text
```

Here, `"Hello, World!"` is to the right of `-<`; it is an input to the arrow. Values passed as input to arrows are known
only while evaluating the handler with a request. On the other hand, `HTTP.ok200` and `"text/plain"` are to the left of
`-<`. Hence, they are known even without evaluating the handler.

This separation of static and dynamic parts of the API enables WebGear to extract static information from handlers
without "executing" them. For example, `webgear-openapi` generates OpenAPI documentation from handlers while
`webgear-server` runs a handler as a WAI application.

WebGear handlers are instances of `Handler` typeclass defined as:

```haskell
class (ArrowChoice h, ArrowPlus h, ArrowError RouteMismatch h, Monad m) => Handler h m | h -> m where
  -- | Lift a monadic function to a handler arrow
  arrM :: (a -> m b) -> h a b
  ....
```

Here, `h a b` is an arrow whose input is of type `a` and output is of type `b`. This is analogous to a function of type
`a -> m b`. An arrow of type `Handler h => h Request Response` will be able to handle HTTP requests and produce a
response.

Every handler has an underlying monad `m` in which the handler executes. You can lift a monadic computation to an arrow
with the `arrM` function.

A handler is an instance of `ArrowChoice`. Thus, we can use conditionals - `if` and `case` expressions - in handler
implementations. A handler also has an instance of `ArrowPlus`. This is used to implement routing - choosing one handler
from many based on the request path and method.

## Constraints

Type annotations on handlers can often be very verbose because you need to explicitly mention all the traits used by the
handlers. WebGear has defined a few type aliases to make it as concise as possible.

As an example, you can use this type annotation if you have a handler arrow `myHandler` that deals with a bunch of
traits:

```haskell
myHandler ::
  ( HaveTraits [q1, q2, q3, ....] req
  , StdHandler h m [t1, t2, t3, ....] [s1, s2, s3, ....]
  ) =>
  RequestHandler h req
myHandler = proc request -> do
  ....
```

The `HaveTraits` constraint requires that the input request has all the traits `q1`, `q2`, `q3`, etc. linked with
it. Typically, this is achieved by wrapping `myHandler` with some middlewares that probe for these traits. You will
learn about middlewares in the next chapter.

The `StdHandler` constraint declares that `myHandler` attempts the following:

* Get the traits `t1`, `t2`, `t3`, etc. from the request using `probe` function.
* Set the traits `s1`, `s2`, `s3`, etc. on the response using `plant` function.

Using `StdHandler` requires less number of constraints than using `Get` and `Set` constraints for each trait. You might
still end up with a single large constraint if the type level lists of traits are very long. In such cases, you can
define a type alias for the list of types - such as `[t1, t2, t3, ....]` - and use the alias in the constraint.

{% include prev-next.html %}

[^1]: See the [arrows tutorial]({% link user_guide/1.0.0/arrows.md %})
