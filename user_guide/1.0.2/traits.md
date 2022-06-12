---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

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
* The type system ensures that we cannot accidentally invoke the wrong handler in both cases.

Traits are a mechanism to achieve this.

## Basics

Traits are optional attributes associated with a value. For example, the JSON formatted body mentioned above is a trait
associated with the request value. In WebGear, it is represented by the type `JSONBody b` - the request body is parsed
into a value of type `b`.

A trait might be associated with many types of values. For example, the `JSONBody b` trait is associated with both
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

Instances for `JSONBody b` are:

```haskell
instance Trait (JSONBody b) Request where
  type Attribute (JSONBody b) Request = b

instance TraitAbsence (JSONBody b) Request where
  type Absence (JSONBody b) Request = Text
```

Here, `Attribute (JSONBody b) Request` is `b` because we get a value of type `b` when we successfully parse the JSON
body. `Absence (JSONBody b) Request` is `Text` because that is the type of error we get when the request body is parsed.

## Linked Values

The next step is to link traits with values. We get a value of type `Linked '[JSONBody b] Request` if we link the
`JSONBody b` type with `Request`. We can link more than one trait - e.g. `t1`, `t2`, `t3`, ... - in which case we'll
have a value of type `Linked '[t1, t2, t3] Request`.[^1]

But what does it mean to have a linked value? A link is a proof that the specified traits exist in the value. If you
have a `Linked '[JSONBody b] Request` value, you have a guarantee that it contains a JSON formatted `b` value. You can
access the trait attribute from a linked value thus:

```haskell
request :: Linked '[JSONBody b] Request
request = ....

let body :: b
    body = pick @(JSONBody b) (from request)
```

You don't need to handle the case of missing JSON body if you have a linked request.

How do we produce a linked value? After all, we need to have a check somewhere that the body is actually present. This
is done with the `probe` function.

```haskell
probe :: Get h t a => t -> h (Linked ts a) (Either (Absence t a) (Linked (t : ts) a))
```

This function gives us an arrow[^2] that takes a linked value - `Linked ts a` - as input and returns another linked
value - `Linked (t:ts) a` - on success or an `Absence t a` value on failure to retrieve the `t` trait. The resultant
linked value has the same list of traits as the original, except for the additional `t` trait.

This means that we can chain many invocations of `probe` for many traits, accumulating all those traits in the `ts`
type-level list. The final linked value is proven to have all those traits. And we can invoke `pick @t (from request)`
for any trait as long as it is present in the `ts` list.

## Getting Traits

A trait has to implement the `Get` typeclass for the `probe` function to work:

```haskell
class (Arrow h, TraitAbsence t a) => Get h t a where
  getTrait :: t -> h (Linked ts a) (Either (Absence t a) (Attribute t a))
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
    -- | A function to generate a linked value. This function must be
    -- called by the `setTrait` implementation to generate a linked
    -- value.
    (Linked ts a -> a -> Attribute t a -> Linked (t : ts) a) ->
    -- | An arrow that attches a new trait attribute to a value linked
    -- with other traits
    h (Linked ts a, Attribute t a) (Linked (t : ts) a)
```

You can use the `plant` function to set a trait attribute on a linked value. This is the counterpart of `probe`.

```haskell
plant :: Set h t a => t -> h (Linked ts a, Attribute t a) (Linked (t : ts) a)
```

This function gives you an arrow that takes a linked value - `Linked ts a` and a trait attribute as inputs and returns
another linked value - `Linked (t:ts) a` - as the result. This result has an additional trait - `t` - on it.

## Final Points

You have seen how traits help to build linked values, which then can be used to access the trait attributes in a
type-safe fashion. But they look like just getters and setters, why do we even need them?

The differentiating factor is that the `Get` and `Set` typeclasses also depend on the arrow in which they operate. This
allows us to have different implementations for the same trait depending on the arrow. For example, there is an
implementation of `JSONBody` for the `ServerHandler` arrow and a completely different one for the `OpenApiHandler`
arrow. The former one implements a server that retrieves a body from the request, while the latter generates OpenAPI
documentation for the body.

In addition to the core traits provided by WebGear, you can have your own traits by implementing the typeclasses
mentioned in this chapter. These traits will be as first class as the WebGear traits.

{% include prev-next.html %}

[^1]: See the DataKinds section in [Language Extensions]({% link user_guide/1.0.2/lang-exts.md %}) chapter.
[^2]: See [arrows tutorial]({% link user_guide/1.0.2/arrows.md %})
