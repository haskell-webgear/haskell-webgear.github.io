---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Routing

So far we have been using a single handler. What if we have multiple API endpoints and corresponding handlers?

## ArrowPlus Typeclass

The [ArrowPlus](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Arrow.html#t:ArrowPlus) typeclass is used
to combine multiple arrows into one. Handlers have instances of `ArrowPlus` and use this mechanism for routing. When
multiple handlers are combined with `<+>`, they are tried from left to right till any of them successfully returns a
response.

Here is an example:

```haskell
allRoutes =
      [route| POST /api/users            |] User.create
  <+> [route| POST /api/users/login      |] User.login
  <+> [route| GET  /api/users/userId:Int |] User.get
  <+> [route| PUT  /api/users/userId:Int |] User.update
```

A handler can indicate a routing failure by invoking the `routeMismatch` arrow. Out of the box, `method`, `path`,
`pathVar`, and `pathEnd` middlewares do this when the request method or path components do not match.

If none of the handlers match a request, an HTTP response with 404 status is returned.

## Routing Middlewares

There are two ways of routing a request. The simplest approach is to use a Template Haskell quasi-quoter as shown
above. There are two quasi-quoters - `route` and `match`. Both of these can take either a path or a method along with a
path as arguments. The `route` quasi-quoter matches the complete route from the request, while `match` matches only a
prefix. More details about these can be found in the API documentation of `WebGear.Core.Trait.Path` module.

There is an alternative, if you prefer to avoid Template Haskell. Both these quasi-quoters are implemented using
middlewares that you can directly use instead:

* The `method` middleware attempts to match an HTTP method.
* The `path` middleware matches a literal path component.
* The `pathVar @"varName" @t` middleware will try to capture the next path component as a variable with the name
  `varName` and convert it to a value of type `t`.
* Finally, `pathEnd` will try to ensure that no more path components remain to be matched in the request.
* All these middlewares will invoke `routeMismatch` on failure.

For example, `[route| GET /api/users/userId:Int |] User.get` can be written as:

```haskell
method GET $ path "/api/users" $ pathVar @"userId" @Int $ pathEnd $ User.get
```

And `[match| GET /api/users/userId:Int |] User.get` translates to:

```haskell
method GET $ path "/api/users" $ pathVar @"userId" @Int $ User.get
```

{% include prev-next.html %}
