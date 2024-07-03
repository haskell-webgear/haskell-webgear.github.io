# Handlers and Middlewares
Now that you've learned how routing works, it is time to learn about request handlers.

A typical web application will extract some data from the request, do some processing, and then produce a
response. WebGear request handlers help to implement all these three steps in a convenient, type-safe manner. Let us see
how.

## Request Handlers
A request handler is an arrow that takes a request as input and produces a response as output. You already saw a few
handlers in this tutorial. They have a few properties that you should learn to use WebGear effectively.

Request handlers represent an abstract computation that can derive a response based on a request. This abstract
computation can be converted to a concrete web application by passing it to the `#!hs toApplication` function. You
already saw the usage of `#!hs toApplication` in previous chapters, but we never mentioned that its argument is a
handler.

Request handlers are composable. In the [routing](./routing.md#multiple-endpoints) chapter, you saw that multiple
endpoints can be combined with the `#!hs <+>` operator to produce a single handler.

## Middlewares
A middleware is a function that takes a handler as input and produces another handler as output. You've encountered a
few middlewares already even though they weren't explicitly labelled so. The `#!hs method`, `#!hs path`, `#!hs pathVar`,
and `#!hs pathEnd` functions from the [routing](./routing.md) are examples of middlewares. They accept a handler as
input and invokes it only if some criteria is met.

Since middlewares return a handler, we can chain them together and produce a handler as shown in the example below:

```haskell
method HTTP.GET $ path "/api/user" $ pathVar @"userId" @Int $ pathEnd userHandler
```

## Traits
WebGear has many more middlewares, we'll explore them in the next chapter. For now, let us turn our attention to the
`#!hs pathVar` middleware. In the code above, `#!hs pathVar @"userId" @Int` extracts a path variable of type `#!hs
Int`. How can we use this variable in our handler?

```haskell
method HTTP.GET $ path "/api/user" $ pathVar @"userId" @Int $ pathEnd $
  proc request -> do
    let userId :: Int
        userId = pick @(PathVar "userId" Int) $ from request
    respondA HTTP.ok200 PlainText -< "ID = " <> show userId
```

You just *pick* the value from the request. But how do we know that the request contains such a path variable? After
all, the request can contain a completely different path. Note that the type of `#!hs userId` is `#!hs Int`. Shouldn't
it be `#!hs Maybe Int` to accommodate the case where the request does not have the path variable?

Let us experiment a little bit; what if you remove the `#!hs pathVar` middleware from the code?

```haskell
method HTTP.GET $ path "/api/user" $ pathEnd $
  proc request -> do
    let userId :: Int
        userId = pick @(PathVar "userId" Int) $ from request
    respondA HTTP.ok200 PlainText -< "ID = " <> show userId
```

Try compiling this code and you'll get an error:

```
• The value doesn't have the ‘PathVar "userId" Int’ trait.
      
  Did you forget to apply an appropriate middleware?
  ...
  
  or did you use a wrong trait type?
  ...
```

That's very impressive. WebGear stops us from using an attribute that could be missing from the request. We can't pick
the path variable from the request unless we have used the appropriate middleware. How does that work? What is this
*trait* mentioned in the error message?

It'll be clearer if we add a type signature to the handler:

```haskell
myApp :: Wai.Application
myApp = toApplication $
  method HTTP.GET $ path "/api/user" $ pathVar @"userId" @Int $ pathEnd userHandler

userHandler :: HasTrait (PathVar "userId" Int) ts => ServerHandler IO (Request `With` ts) Response
userHandler = proc request -> do
  let userId :: Int
      userId = pick @(PathVar "userId" Int) $ from request
  respondA HTTP.ok200 PlainText -< "ID = " <> show userId
```

The type of `#!hs userHandler` is `#!hs ServerHandler IO (Request `With` ts) Response`. Here, `ServerHandler IO` is the
type of the arrow used as the handler. The arrow input is ``#!hs Request `With` ts`` and the output is `#!hs Response`.

In this type, `#!hs ts` is a list of trait types. You might be wondering, what is a trait? Simply put, traits are
attributes associated with a request. `#!hs PathVar "userId" Int` is one such trait that represents a path variable. The
`#!hs ts` type variable contains all traits that are known to be present in the request. The `#!hs HasTrait (PathVar
"userId" Int) ts` constraint is an assertion that the path variable trait definitely exists in the list `#!hs ts`. That
is why we can use `#!hs pick @(PathVar "userId" Int)` to get a user ID without having to handle the case of a missing
path variable.

In order to satisfy the `#!hs HasTrait` constraint, we must use the `#!hs pathVar` middleware. If you omit that
middleware, the constraint will not be satisfied and GHC will show an error message as shown above.

In the rest of this tutorial, you will see this pattern used again and again. Your handler will be wrapped in an
appropriate middleware and then the handler can access trait using the `#!hs pick` function.

## A Better Type Signature
The type signature of `userHandler` given above works fine. However, WebGear handlers tend to use a more general type:

```haskell
userHandler :: (StdHandler h m, HasTrait (PathVar "userId" Int) ts) => RequestHandler h ts
```

`#!hs RequestHandler h ts` is a type synonym and is equivalent to ``#!hs h (Request `With` ts) Response``. Doesn't that
look very similar to ``#!hs ServerHandler IO (Request `With` ts) Response``? Of course, it does. The main change we did
is to replace `#!hs ServerHandler IO` with a type variable `#!hs h`.

As mentioned above, `#!hs ServerHandler IO` is the type of the arrow used by `#!hs userHandler`. This arrow allows us to
convert the handler to a Wai application using the `#!hs toApplication` function. But WebGear handlers are not limited
to just being an application. In a later chapter, you'll learn how to generate OpenAPI or Swagger documentation from
these handlers. In order to support all such use cases, we switch to a polymorphic type variable `#!hs h` instead of the
concrete arrow type `#!hs ServerHandler IO`. WebGear can substitute an appropriate type in place of `#!hs h` depending
on the use case.

As a result of this change, we need an additional constraint `#!hs StdHandler h m` in the type signature. This
constraint enforces that `#!hs h` is a handler arrow and has capabilities that all WebGear handlers are required to
have. For example, handler arrows can embed monadic actions in them. We'll see more on that in the next section.

Going forward, we will use this polymorphic type signature for all our handlers.

## Monadic Actions in Handlers
Most WebGear handlers will need some monadic actions to process the request. As an example, let us consider a handler
that returns the current date and time in UTC. We can use the `#!hs Data.Time.getCurrentTime` function for this
pupose. But, that function has the type `#!hs IO UTCTime`. How do we add it to a handler which is an arrow and not a
monad?

It is fairly straightforward. All handlers allow embedding monadic actions in them using a function `#!hs arrM`. This is
how you'd use it:

```haskell
import Data.Time
import Control.Monad.IO.Class

currentTimeHandler ::
  forall h m ts.
  (StdHandler h m, MonadIO m) =>
  RequestHandler h ts
currentTimeHandler = proc request -> do
  now <- arrM getNow -< ()
  respondA HTTP.ok200 PlainText -< show now
  where
    getNow :: () -> m UTCTime
    getNow () = liftIO getCurrentTime
```

If you have a function of type `#!hs a -> m b` where `#!hs m` is a monad, the function `#!hs arrM` can *lift* it to a
handler arrow of type `#!hs StdHandler h m => h a b`. In the above example, we lifted the `#!hs getNow` function to an
arrow of type `#!hs h () UTCTime` and used it inside the `#!hs proc` body.

Unlike monads, handlers (and arrows in general) require both an input and an output. The `#!hs getCurrentTime` IO action
does not require any inputs and we should convert it to a function by passing `#!hs ()` as an input.

## Bring Your Own Monad
Notice how we used the `#!hs MonadIO m` constraint instead of using the concrete `#!hs IO` type. Using a polymorphic
type helps us to switch to a different monad if required without modifying the handler.

You may use any monad of your choice with request handlers. However, the `#!hs toApplication` function requires the
handler to be based on the `#hs IO` monad. So, how do you convert handlers based on your custom monad stack to a Wai
application?

The answer is to use the `#!hs transform` function to convert your monad stack. Let us assume you have a [ReaderT
stack](https://tech.fpcomplete.com/blog/2017/06/readert-design-pattern/). This is how you generate a Wai application in
that case:

```haskell
apiHandler :: StdHandler h (ReaderT Env IO) => RequestHandler h ts
apiHandler = ...

application :: Env -> Wai.Application
application env = toApplication $ transform appToIO apiHandler
  where
    appToIO :: ReaderT Env a -> IO a
    appToIO f = runReaderT f env
```

The `#!hs transform` function has the following type:

```haskell
transform :: (forall x. m x -> n x) -> ServerHandler m a b -> ServerHandler n a b
```

Given a function - such as `#!hs appToIO` above - that transforms one monadic action to another, it can transform
between the corresponding `#!hs ServerHandler` types. You then pass the transformed handler to `#!hs toApplication`.

## Summary
In this chapter, you learned the basics of request handlers, middlewares, and traits. By now, you have a good
understanding of how traits help in accessing request attributes in a type safe manner. You also learned to write
polymorphic type signatures for request handlers.

In the next chapter we'll meet many more commonly used middlewares and traits.
