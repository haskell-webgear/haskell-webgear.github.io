# Generating Responses

WebGear has a number of middlewares and functions to generate responses. We'll explore them in this chapter.

## Basic Response Generation

The easiest way to generate responses is using the `respondA` arrow. For example, to generate a plain text response with
HTTP status 200:

```haskell
import qualified Network.HTTP.Types as HTTP

respondA HTTP.ok200 "text/plain" -< "A response body"
```

The `respondA` arrow generates a `Linked res Response`. You can compose it with `unlinkA` to convert it to a `Response`.

For generating JSON responses, you can use `respondJsonA`:

```haskell
data Person = Person { name :: Text, email :: Text }
  deriving (Generic, ToJSON)

let person :: Person
    person = Person "John Smith" "john@smith.com"

unlinkA <<< respondJsonA HTTP.ok200 -< person
```

## Advanced Response Generation

You can get finer control over the response using a few additional middlewares and arrows.

The `mkResponse` arrow generates a response with a specific HTTP status code.

```haskell
-- resp :: Linked '[Status] Response
resp <- mkResponse HTTP.ok200 -< ()
```

There are helper arrows in the `WebGear.Core.Trait.Status` module that generates responses for all standard HTTP status
codes.

The `setBody` and `setJSONBody` middlewares can add a body to a response. For example:

```haskell
setBody "text/plain" ok200 -< ("Hello, World!", ())
```

```haskell
let person :: Person
    person = Person "John Smith" "john@smith.com"

setJSONBody ok200 -< (person, ())
```

The `setHeader` middleware adds a header value to a response:

```haskell
setHeader @"Cache-Control" @Text ok200 -< ("no-cache", ())
```
