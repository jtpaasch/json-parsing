# Aeson tutorial

When you use Aeson, `OverloadedStrings` should be enabled, at the top of
the file.

To play with examples, you can run ghci, and set overloaded strings.

```
>>> :set -XOverloadedStrings
```

I assume Aeson is imported qualified as `J`:

```
>>> import qualified Data.Aeson as J
```


## Encoding (going from Haskell to JSON)

Aeson knows how to convert simple Haskell data into JSON data.
Use the `J.encode` function to do this.

For example, convert the integer 1 into JSON:

```
>>> J.encode 1
"1"
```

Notice that `J.encode` spits out a string, which is just what we expect,
since JSON is a string. Actually, that's not right. It's a bytestring,
which is what we really expect, since JSON is a stream of bytes.

```
>>> :t (J.encode 1)
(J.encode 1) :: Data.ByteString.Lazy.Internal.ByteString
```

Convert a list of numbers into JSON:

```
>>> J.encode [1, 2, 3]
"[1, 2, 3]"
```

Again, notice that `encode` returns a bytestring, which is exactly the
correct string of JSON we would expect.

Convert a boolean to JSON:

```
>>> J.encode True
"true"
```

Convert a string to JSON:

```
>>> J.encode "foo"
"\"foo\""
```

Notice that `foo` is enclosed in quotations, which is exactly how a
string should be presented in JSON. The quotes are escaped here, but
that's just part of ghci's display. The actual data that is returned
is just `foo`, surrounded in quotes.

Convert a list of strings into a JSON array:

```
>>> J.encode ["foo", "bar"]
"[\"foo\",\"bar\"]"
```

What about `Nothing :: Maybe Int`?

```
>>> J.encode (Nothing :: Maybe Int)
>>> "null"
```

Records can be encoded as objects too, but not without a little boilerplate.
I'll discuss that later.


## Decoding (going from JSON to Haskell)

What about decoding from JSON into Haskell? Use the `J.decode` function.
This returns a `Maybe` result. If the decoding process fails (because of,
say, malformed or unexpected JSON), then it returns `Nothing`. If it succeeds,
you get `Just x`, where `x` is the result.

The trick is, we need to tell Aeson what type of value it is converting
the JSON bytestring into, but wrapped in `Maybe`.

For example, to decode `1` into an `Integer`:

```
>>> J.decode "1" :: Maybe Integer
Just 1
```

We can decode that into a float, too:

```
>>> J.decode "1" :: Maybe Float
Just 1.0
```

Try to decode `"abc"` into an `Integer` (remember to escape the quotes):

```
>>> J.decode "\"abc\"" :: Maybe Integer
Nothing
```

Yep, it can't do it, so we get back nothing.

We can see why it failed if we use `J.eitherDecode`,
which returns either the error as a string, or the resulting data type.

```
>>> J.eitherDecode "\"abc\"" :: Either String Integer
Left "Error in $: parsing Integer failed, expected Number, but encountered String"
```

That makes sense. With our type annotation we instructed Aeson to give us
an integer, but it got a string instead.

Notice that we put quotes around `abc`. That's because, in JSON,
strings have to be in quotes.

What happens if we try to decode `abc`, without the quotes? 

```
>>> J.eitherDecode "abc" :: Either String Integer
Left "Error in $: Failed reading: not a valid json value at 'abc'"
```

That's as we expect. As I just noted, in JSON, strings have to be
quoted. The raw sequence of characters `abc` is not a valid JSON value.

Let's convert `"abc"` into a string:

```
>>> J.decode "\"abc\"" :: Maybe String
Just "abc"
```

JSON allows you to mix different types of values in an array.
Haskell does not. So what type would we convert this into:

```
>>> J.decode "[1, true]" :: Maybe ??
```

We need to use tuples for this kind of thing:

```
>>> J.decode "[1, true]" :: Maybe (Integer, Bool)
Just (1, True)
```

We can decode more nested arrays too in this way. For example:

```
>>> J.decode "[1,[\"a\",\"b\"]]" :: Maybe (Integer, (String, String))
Just (1,("a","b"))
```


## Manual JSON

Aeson has a datatype for JSON, which is called `Value`, and it has the
following straightforward constructors:

* `Object (HashMap Text Value)`
* `Array (Vector Value)`
* `String Text`
* `Number Scientific`
* `Bool Bool`
* `Null`

You can build your own JSON data, using these constructors.

```
>>> J.encode $ J.Null
"null"

>>> J.encode $ J.Bool True
"true"

>>> J.encode $ J.Number 2.5
"2.5"

>>> J.encode $ J.String "Hello world"
"\"Hello world\""
```

To construct an array, you need to construct a vector.

```
>>> import qualified Data.Vector as V
>>> let myNums = V.fromList [J.Number 1, J.Number 2]
```

Then construct a JSON array from that vector:

```
>>> J.encode $ J.Array myNums
"[1, 2]"
```

To construct an object, build a hashmap:

```
>>> import qualified Data.HashMap.Strict as HM
>>> let objFields = HM.fromList [("foo", J.Number 10), ("bar", J.Bool True)]
```

Then make a JSON object from it:

```
>>> J.encode $ J.Object objFields
"{\"foo\":10,\"bar\":true}"
```


## Parsing JSON

Suppose we want to parse a JSON value. We need to handle each case.
The value can be a string, a number, a bool, etc.

Let's write a function to parse a boolean JSON value. The function will
take a JSON value, and returns either a string error (if the value isn't
a boolean), or a boolean.

```
parseBool :: J.Value -> Either String Bool
parseBool (J.Bool b) = Right b
parseBool _ = Left "Expected a bool"
```

Here you can see that, if `J.Value` is a `J.Bool`, we return the value.
Otherwise, we return a string error.

We can do the same for strings (which are `Text`):

```
import qualified Data.Text as T
parseText :: J.Value -> Either String String
parseText (J.String x) = Right $ T.unpack x
parseText _ = Left "Expected string"
```

And we could do something similar for numbers. The `J.Number` type works
with scientific numbers, so we could use the functions in `Data.Scientific`
to parse out an Integer, or a Float.






## Parsing JSON Manually

Now let's parse some JSON manually. Suppose we have a simple JSON object:

```
>>> let objFields = HM.fromList [("foo", J.Number 10), ("bar", J.Bool True)]
>>> let obj = J.Object objFields
>>> J.encode obj
"{\"foo\":10,\"bar\":true}
```

Let's write a function to parse this manually.
Suppose we want to look in the object, and get the value for the
field 'foo'. If there's a problem, we'll return a string error.
So the type is, we take a JSON value, and then we return either
an error (a string) or the retrieved JSON value:

```
parse :: J.Value -> Either String J.Value
```

Let's write the case for when the function input is `J.Object`:

```
-- If it's an object, let's try to find our value.
parse (J.Object obj) =

  -- See if there's a 'foo' field in the hashmap.
  -- If not, return an error. If so, return the value.
  let fooField = HM.lookup "foo" obj
  in case fooField of
    Nothing -> Left "No field 'foo'."
    Just foo -> Right foo
```

For other types of JSON values, we want to error:

```
parse _ = Left "Must be an object."
```

Try it:

```
>>> parse (J.Number 1)
Left "Must be an object"

>>> parse obj
Right (Number 10.0)
```

Let's suppose now we want to look up the value of "bar" in the same object,
assuming that we successfully bound "foo". We'll modify `parse (J.Object obj)`.

```
-- If it's an object, let's try to find our value.
parse (J.Object obj) =

  -- See if there's a 'foo' field in the hashmap.
  -- If not, return an error. If so, return the value.
  let fooField = HM.lookup "foo" obj of
  in case fooField of
    Nothing -> Left "No field 'foo'."
    Just foo ->

      -- Now see if there's a 'bar' field. If not error.
      let barField = HM.lookup "bar" obj
      in case barField of
        Nothing -> Left "No field 'bar'."
        Just bar -> Right bar
```

It's obviously tedious to nest these lookups. We can use monads to
sequence them.

```
parse (J.Object obj) = do

  -- Look for 'foo'
  let fooField = HM.lookup "foo" obj
  foo <- case fooField of
    Nothing -> Left "No field 'foo'"
    Just x -> return x

  -- Look for 'bar'
  let barField = HM.lookup "bar" obj
  bar <- case barField of
    Nothing -> Left "No field 'bar'"
    Just x -> return x

  -- Return what we found.
  Right bar
```

We can check for other things too, like the type of value.
Suppose we need 'bar' to be a boolean:

parse (J.Object obj) = do

  -- Look for 'foo'
  let fooField = HM.lookup "foo" obj
  foo <- case fooField of
    Nothing -> Left "No field 'foo'"
    Just x -> return x

  -- Look for 'bar'
  let barField = HM.lookup "bar" obj
  bar <- case barField of
    Nothing -> Left "No field 'bar'"
    Just (J.Bool x) -> return x
    Just _ -> Left "'bar' must be bool"

  -- Return what we found.
  Right bar

-}
