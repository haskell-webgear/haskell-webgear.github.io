# Traits

*Traits* are a fundamental concept in WebGear that enables type-safety in APIs and many other elements make use of it
one way or another. So let us explore it first.

## Motivation

Most APIs handlers retrieve some information from the request and process it. For example, consider an endpoint that
expects a JSON formatted request body. The body may not be present in the request, in which case we need to perform some
error handling. We might have many endpoints that share the same error handling logic. So it is useful if we could build
our handlers such that:

* The request handler only has to deal with the cases where all required information is guaranteed to be present.
* The error handler only has to deal with the cases where the required information is absent.
* The type system ensures that we cannot accidentally invoke the wrong handler in both these cases.

Traits are a mechanism to achieve this.

## Basics

Traits are optional attributes associated with a value. For example, the JSON formatted body mentioned above is a trait
associated with the request value. In WebGear, it is represented by the type `Body JSON b` - the request body is parsed
into a value of type `b`.

A trait might be associated with many types of values. For example, the `Body JSON b` trait is associated with both
`Request` and `Response` types because both may contain a body.

Traits have instances of these typeclasses:

```haskell
-- Indicates that `t' is contained in `a'
class Trait t a where
  -- Type of trait value
  type Attribute t a :: Type

class Trait t a => TraitAbsence t a where
  -- An error indicator when the trait is missing
  type Absence t a :: Type
```

Instances for `Body JSON b` are: (1)
{ .annotate }

1. This is a simplified example. The actual definitions of these instances is more general.

```haskell
instance Trait (Body JSON b) Request where
  type Attribute (Body JSON b) Request = b

instance TraitAbsence (Body JSON b) Request where
  type Absence (Body JSON b) Request = Text
```

Here, `Attribute (Body JSON b) Request` is `b` because we get a value of type `b` when we successfully parse the JSON
body. `Absence (Body JSON b) Request` is `Text` because that is the type of error we get when the request body is parsed.

## Witnessed Values

The next step is to associate traits with values. We get a value of type ``Request `With` '[Body JSON b]`` if we
associate the `Body JSON b` type with `Request`. We can associate more than one trait - e.g. `t1`, `t2`, `t3`, ... - in
which case we'll have a value of type ``Request `With` '[t1, t2, t3]`` (1) . These are called witnessed values.
{ .annotate }

1. See the DataKinds section in [Language Extensions](../reference/language-extensions.md#datakinds) chapter.

But what does it mean to have a witnessed value? A witness is a proof that the specified traits exist in the value. If you
have a ``Request `With` '[Body JSON b]`` value, you have a guarantee that it contains a JSON formatted `b` value. You can
access the trait attribute from a witnessed value thus:

```haskell
request :: Request `With` '[Body JSON b]
request = ....

let body :: b
    body = pick @(Body JSON b) $ from request
```

You don't need to handle the case of missing JSON body if you have a witnessed request.

How do we produce a witnessed value? After all, we need to have a check somewhere that the body is actually
present. This is done with the `probe` function.

```haskell
probe :: Get h t a => t -> h (a `With` ts) (Either (Absence t a) (a `With` (t : ts)))
```

This function gives us an arrow (1) that takes a witnessed value - ``a `With` ts`` - as input and returns another
witnessed value - ``a `With` (t:ts)`` - on success or an `Absence t a` value on failure to retrieve the `t` trait. The
resultant witnessed value has a `t` trait in addition to the traits in the original request.
{ .annotate }

1. See [arrows tutorial](../reference/arrows-tutorial.md)

We can chain many invocations of `probe` for many traits, accumulating all those traits in the `ts` type-level list. The
final witnessed value will have all those traits. And we can invoke `pick @t $ from request` for any trait as long as it
is present in the `ts` list.

## Getting Traits

A trait has to implement the `Get` typeclass for the `probe` function to work:

```haskell
class (Arrow h, TraitAbsence t a) => Get h t a where
  getTrait :: t -> h (a `With` ts) (Either (Absence t a) (Attribute t a))
```

The `getTrait` should attempt to retrieve the trait attribute or return an `Absence` value to indicate failure.

You can build your own traits by providing instances of `Trait`, `TraitAbsence`, and `Get`.

## Setting Traits

Everything we discussed so far was to retrieve traits from requests. A similar implementation exists for setting traits
on responses as well. The `Set` typeclass exists for this purpose:

```haskell
class (Arrow h, Trait t a) => Set h t a where
  -- | Set a trait attribute @t@ on the value @a@.
  setTrait ::
    -- | The trait to set
    t ->
    -- | A function to generate a witnessed value. This function must be
    -- called by the `setTrait` implementation to generate a witnessed
    -- value.
    (a `With` ts -> a -> Attribute t a -> a `With` (t : ts)) ->
    -- | An arrow that attches a new trait attribute to a value witnessed
    -- with other traits
    h (a `With` ts, Attribute t a) (a `With` (t : ts))
```

You can use the `plant` function to set a trait attribute on a witnessed value. This is the counterpart of `probe`.

```haskell
plant :: Set h t a => t -> h (a `With` ts, Attribute t a) (a `With` (t : ts))
```

This function gives you an arrow that takes a witnessed value - ``a `With` ts`` and a trait attribute as inputs and returns
another witnessed value - ``a `With` (t:ts)`` - as the result. This result has an additional trait - `t` - on it.

## Final Points

You have seen how traits help to build witnessed values, which then can be used to access the trait attributes in a
type-safe fashion. But they look like just getters and setters, why do we even need them?

The differentiating factor is that the `Get` and `Set` typeclasses also depend on the arrow in which they operate. This
allows us to have different implementations for the same trait depending on the arrow. For example, there is an
implementation of `Body JSON` for the `ServerHandler` arrow and a completely different one for the `OpenApiHandler`
arrow. The former one implements a server that retrieves a body from the request, while the latter generates OpenAPI
documentation for the body.

In addition to the core traits provided by WebGear, you can have your own traits by implementing the typeclasses
mentioned in this chapter. These traits will be as first class as the WebGear traits.
