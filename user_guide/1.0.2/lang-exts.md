---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Language Extensions

This chapter has a quick summary of some of the advanced extensions that WebGear makes use of. The [GHC User's
Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html) has more information on these extensions.

## Arrows

This extension enables the `proc` notation for arrows. The [arrows tutorial]({% link user_guide/1.0.2/arrows.md %})
explains this notation in detail.

## DataKinds

This extension promotes data types to kind level and enables literals at type level. For example, the `QueryParam` trait
is defined as:

```haskell
import GHC.TypeLits (Symbol)

data Existence = Required | Optional

data ParseStyle = Strict | Lenient

data QueryParam (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type) = QueryParam
```

This lets us write types such as `QueryParam Required Strict "offset" Integer`. The data constructors `Required` and
`Strict` got promoted to a type, and the types `Existence` and `ParseStyle` got promoted to kinds. This also allows
string literals such as `"offset"` as a type.

This extension also allows using lists at the type level. For example, types such as `Linked [RequiredHeader
"Content-Type" Text, JSONBody ErrorResponse] Response` is possible because of this extension.

## TypeApplications

WebGear uses this extension to explicitly mention values of type variables in some cases. For example, to extract a
trait attribute from a linked request:

```haskell
let maybeAuthor :: Maybe Text
    maybeAuthor = pick @(OptionalQueryParam "author" Text) $ from request
```

The `@(OptionalQueryParam "author" Text)` type application specifies the trait to retrieve.

## QuasiQuotes

This extension enables the Template Haskell quasi-quotation syntax. WebGear uses this syntax to enable routing
middlewares such as `route` and `match`. Their usage is described in the [Routing
chapter](./routing.html#routing-middlewares).

Usage of this extension is optional. You can implement equivalent functionality without using Template Haskell at the
cost of some verbose code.

{% include prev-next.html %}
