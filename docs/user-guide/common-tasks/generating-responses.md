# Generating Responses

WebGear has a number of middlewares and functions to generate responses. We'll explore them in this chapter.

## Basic Response Generation

The easiest way to generate responses is using the `respondA` arrow. For example, to generate a plain text response with
HTTP status 200:

```haskell
import qualified Network.HTTP.Types as HTTP

respondA HTTP.ok200 PlainText -< "A response body" :: Text
```

For generating JSON responses:

```haskell
data Person = Person { name :: Text, email :: Text }
  deriving (Generic, ToJSON)

let person :: Person
    person = Person "John Smith" "john@smith.com"

respondA HTTP.ok200 JSON -< person
```

## Advanced Response Generation

You can get finer control over the response using a few additional control structures.

The `mkResponse` arrow generates a response with a specific HTTP status code.

```haskell
-- resp :: Response `With` '[Status]
resp <- mkResponse HTTP.ok200 -< ()
```

There are helper arrows in the `WebGear.Core.Trait.Status` module that generates responses for all standard HTTP status
codes.

`setBody` can add a body to a response. For example:

```haskell
(ok200 -< ())
  >-> (\resp -> setBody PlainText -< (response, "Hello, World!" :: Text))
```

`setHeader` adds a header value to a response:

```haskell
(ok200 -< ())
  >-> (\resp -> setHeader @"Cache-Control" @Text -< (resp, "no-cache"))
```
