---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Arrows Tutorial

Arrows - like monads - are an abstraction to represent computations with a context. However, they can express contexts
that are not possible with monads.

WebGear makes extensive use of arrows to build APIs, hence it is important that you have a good grasp of them to
effectively use WebGear.

## Basics

Intuitively, arrows are similar to functions. Like functions, arrows have kind `Type -> Type -> Type`. That is, they
take an input and an output type arguments. Throughout this chapter, we'll use the type variable `h` to indicate an
arrow. Hence, `h a b` is an arrow with `a` as the input type and `b` as the output type.

One significant similarity of arrows and functions is that both are composable. Arrows have
[Category](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Category.html#t:Category) as a
superclass. Category is defined as:

```haskell
class Category cat where
  id :: cat a a                         -- The identity arrow
  (.) :: cat b c -> cat a b -> cat a c  -- Associative composition
```

You are probably already familiar with `id` and `(.)` as identity function and function composition. `Category` is a
generalization of these, it represents all types that can be composed like functions.

There are two composition operators that make the direction of composition explicit:

```haskell
-- Right to left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
-- Left to right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
```

The `Arrow` type class is defined as:

```haskell
class Category h => Arrow h where
  -- convert a function to an arrow
  arr :: (a -> b) -> h a b
  -- map over the input type
  first :: h a b -> h (a, c) (b, c)
  -- map over the output type
  second :: h a b -> h (c, a) (c, b)
  --- combine two arrows
  (***) :: h a b -> h c d -> h (a, c) (b, d)
  (&&&) :: h a b -> h a c -> h a (b, c)
```

These methods allow us to operate on multiple inputs and/or outputs represented as pairs in a single computation.

Note that all operations you saw above manipulate and compose arrows in various ways to produce other arrows. But none
of them allow you to "execute" an arrow - i.e. provide an input to an arrow and get an output. This is not part of the
`Arrow` interface, but a specific arrow type might have such a mechanism.

This is similar to the monad interface. The `Monad` typeclass defines a `return` method to lift a value into monadic
context and `>>=` to bind two monadic computations. But there is no method to extract a value from a monadic
context. Specific monads (for e.g., `Maybe`, `Either`, etc.) might provide some mechanism to extract a value from a
monad. But it is not possible with many other monads (for e.g., `IO`, `Const`, etc.).

In WebGear, you would compose handler arrows to form a single handler. The `ServerHandler` type allows you to provide a
`Request` as input to this arrow and get a `Response` as output. This is how `webgear-server` package implements its API
servers. The `OpenApiHandler` from `webgear-openapi` package do not have any mechanisms to run a server. Instead, it can
extract static information about the APIs to generate OpenAPI documentation.

## Conditionals

The `Arrow` typeclass is not expressive enough to build APIs. We often need to conditionally perform actions in a
handler. This is where `ArrowChoice` typeclass comes in:

```haskell
class Arrow h => ArrowChoice h where
  left  :: h a b -> h (Either a c) (Either b c)          -- maps over left option
  right :: h a b -> h (Either c a) (Either c b)          -- maps over right option
  (+++) :: h a c -> h b d -> h (Either a b) (Either c d) -- maps over both left and right
  (|||) :: h a c -> h b c -> h (Either a b) c            -- (+++), then merge results
```

These arrows operate on `Either` values and choose different arrows to handle `Left` and `Right` cases.

In WebGear, `ArrowChoice` interface is used to generate different responses based on request traits. For example, if a
handle requires a query parameter, we want to generate a 404 response when it is missing. Thus, we need two different
handlers - one for the case where we have the query parameter in the request and another one where we don't. The `|||`
operator achieves this by producing an arrow that takes an `Either a b` value as input and invoking one of the two
handlers based on the input.

## Monoid Operations

There are cases where we want to combine multiple arrows to form a single arrow. Some arrows support this operation with
`ArrowZero` and `ArrowPlus` typeclasses.

```haskell
class Arrow h => ArrowZero h where
  zeroArrow :: h a b
  
class ArrowZero h => ArrowPlus h where
  (<+>) :: h a b -> h a b -> h a b
```

Notice the similarity of these classes to `Semigroup` and `Monoid` respectively. They define an associative binary
operation to combine two arrows and also a `zeroArrow` that acts as an identity for the `<+>` operation. In other words,
for any arrow `h`, you have `h <+> zeroArrow = h` and `zeroArrow <+> h = h`.

In WebGear, these typeclasses are used to implement routing. The `zeroArrow` is a handler that always fails, indicating
a route mismatch so that other handlers can be tried. In case of `f <+> g`, the `f` handler is tried first to get a
response. If that fails to match the route, the `g` handler will be tried next.

## The proc Notation

The arrow combinators are cumbersome to program with in practice. For example, consider this code:

```haskell
squareA :: Arrow h => h Int Int
squareA = arr (\x -> x * x)

pythagorasA :: Arrow h => h (Int, Int) Int
pythagorasA = (squareA *** squareA) >>> arr (uncurry (+))
```

It isn't immediately obvious what `pythagorasA` is trying to do. It'll help if we had some notation to add extra
variable names for intermediate values. That is where `proc` notation comes in. This syntax is enabled by the `Arrows`
language extension. The above program can be written with proc notation as:

```haskell
squareA :: Arrow h => h Int Int
squareA = proc x ->
  returnA -< x * x

pythagorasA :: Arrow h => h (Int, Int) Int
pythagorasA = proc (x, y) -> do
  xSquare <- squareA -< x
  ySquare <- squareA -< y
  returnA -< xSquare + ySquare
```

The proc notation is similar to the monadic do notation. The main difference is that an arrow has both an input and
output components, unlike monad which only has one.

The proc block starts with the `proc` keyword followed by a pattern representing the input to this arrow. For example,
the `pythagorasA` arrow has `(x, y)` as its input. 

The body of the arrow notation is called a command. There are a few possible ways to write a command. In the above code,
`squareA` has a single command `returnA -< x * x` as its body. This means `x * x` is sent as input to `returnA` and its
output is the output of `squareA`. `returnA` is the identity arrow, so effectively `squareA` returns the square of its
input.

In general, this has the form `aexp -< iexp` where `aexp` is an expression evaluating to an arrow and `iexp` is an
expression evaluating to its input.

More complex arrows - such as `pythagorasA` - use a do block. In the first line `xSquare <- squareA -< x`, the value `x`
is input to the arrow `squareA` and the result is `xSquare`. The final line feeds `xSquare + ySquare` to `returnA` which
produces the result of `pythagorasA`.

There are a few restrictions on what you can do with this notation. Firstly, the part to the left of `<-` is a pattern
just like the monadic do notation.

Secondly, the expression between `<-` and `-<` must be an arrow. This expression cannot use any of the variables bound
in the proc notation. For example, the code below is invalid because the `x` is bound as a result in first line and is
used as part of the arrow expression in second line.

```haskell
proc a -> do
  x <- foo -< a
  y <- bar x -< a
  ....
```

Arrows do not let you choose the next arrow to use based on some intermediate result from a previous arrow.[^1] Bound
variables such as `x` and `y` can only be used as an input to other arrows (i.e. on the right side of `-<`).

The proc notation also supports `if` and `case` in commands. These require an instance of `ArrowChoice`:

```haskell
factorialA :: ArrowChoice h => h Int Int
factorialA = proc n ->
  if n == 0
    then returnA -< 1
    else do
      res <- factorialA -< n - 1
      returnA -< n * res

data Result = Found [Pet] | NotFound

searchPets :: ArrowChoice h => h Name Result
searchPets = proc name -> do
  pets <- fetchPetsFromDB -< name
  case pets of
    [] -> returnA -< NotFound
    ps -> returnA -< Found ps
```

## Summary

Arrows let us build composable programs. You can compose many arrows in multiple ways to produce a resultant arrow, and
then in the next step run that arrow. The clean separation between these two steps help WebGear to retain static
information about APIs as well as run it as a server.

See these resources for more information about arrows:

1. [The Haskell wikibook](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) has a chapter on arrows.
2. Haskell wiki has an [arrow tutorial](https://wiki.haskell.org/Arrow_tutorial).
3. These two papers introduce arrows from first principles and are easy to read:
   * [Generalising Monads to Arrows](http://www.cse.chalmers.se/%7Erjmh/Papers/arrows.pdf)
   * [Programming with Arrows](http://www.cse.chalmers.se/%7Erjmh/afp-arrows.pdf)

{% include prev-next.html %}

[^1]: `ArrowApply` typeclass allows this. But such arrows are equivalent to monads and you are better off using monads
    in that case.
