---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Getting Started

## Prerequisites

This guide contains detailed information on how to use WebGear effectively. Still, there are some things you should
learn before using WebGear.

### Haskell

WebGear requires that you have a decent understanding of Haskell programming language. You must have a good grasp of
typeclasses, functors, and monads. You should also be familiar with some commonly used GHC extensions such as
MultiParamTypeClasses, OverloadedStrings, TypeApplications etc.

WebGear uses few other GHC extensions. This guide includes material to teach you those features.

### GHC Requirements

WebGear 1.0.0 supports GHC versions `8.8.x`, `8.10.x`, and `9.0.x`. While other versions are untested and unsupported,
you can try them on your own.

### Build Tools

WebGear will work with both [Stack](https://haskellstack.org/) and [Cabal](https://cabal.readthedocs.io/). You should
know how to use one of these tools to build a WebGear application.

## Getting help

If you run into issues, see the [troubleshooting]({% link user_guide/1.0.0/troubleshooting.md %}) section first. You can
report bugs and request features in the [GitHub repo](https://github.com/haskell-webgear/webgear). If you have more
questions, you can discuss them in the [Haskell subreddit](https://www.reddit.com/r/haskell/).

{% include prev-next.html %}
