---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Introduction

## What is WebGear?

WebGear is a high-performance framework to build composable, type-safe HTTP APIs. It is designed to make common API
development tasks easy. It is also easily extensible to add components needed by your project.

## Why use WebGear?

If you are already familiar with other Haskell web frameworks, you might be interested in how WebGear compares against
them. This section compares WebGear against some popular frameworks to give you an idea of why it is useful.

### Servant

Servant uses advanced type level combinators to express API components, while WebGear has traits which are regular data
types combined with a few type class instances. Most of the differences between WebGear and Servant stem from this. Note
that WebGear do use type-level lists of traits for building type-safe APIs. But that is a fairly limited usage of
type-level programming and is easier to learn.

* Programming at type level is more verbose and require knowledge of advanced extensions. This is avoided in WebGear by
  doing most of the operations at value level instead of type level.
* Some type-level structures are not as composable as regular values. For example, a package like
  [servant-flatten](https://github.com/alpmestan/servant-flatten) is needed to factor out combinators. WebGear does not
  have this problem, simply because it uses function calls and function composition to express APIs.
* It is hard to express certain ideas at type-level. For example, basic authentication support in Servant requires a
  `Context` that specifies a function to validate credentials. It is required to use this ad-hoc approach because
  Servant combinators are at type level and cannot include a function. On the other hand, WebGear traits can include
  functions in them because they are values.
* Servant is a very popular project with a large ecosystem of companion packages. WebGear is a younger project, and may
  not support such a wide variety of functionality.
  
### IHP

IHP is an opinionated full-stack framework for rapid application development. You can build everything from the DB
schema to UI with IHP. It can significantly reduce the development time by using standardized steps to build various
components. However, it may not be suitable for your specific use case and constraints. For example, IHP requires the
use of Nix, and PostgreSQL is the only supported database. Your applications may not always be compatible with such
constraints.

WebGear does not aim to be a full-stack framework. It focuses on doing one thing well - building APIs. You are free to
choose a UI framework, a persistence backend, build tool as your project demands.

### Scotty

Scotty is a minimalist framework to build web applications. It is trivial to build simple web applications in Scotty,
but it does not support many of the WebGear features such as type-safety provided by traits, ability to build reusable
components, automatic generation of OpenAPI documentation etc.

### Other Differences

WebGear uses [arrows](https://www.haskell.org/arrows/) and monads to build APIs, while other frameworks only use
monads. This means there is one additional thing to learn if you are not familiar with arrows. However, in my opinion,
this cost is justified as arrows can express API specifications at value level instead of type level. Arrows and their
`proc` notation can be learned relatively easily as a natural extension of monadic computation. This guide includes an
[arrow tutorial]({% link user_guide/1.0.2/arrows.md %}) to help you if you are unfamiliar with arrows.

{% include prev-next.html %}
