# ReScript JSON Schema

Typesafe JSON Schema for ReScript

## Status

> **rescript-json-schema** is currently in beta. Its core API is useable right now, but you might need to pull request improvements for advanced use cases, or fixes for some bugs. Some of its APIs are not "finalized" and will have breaking changes over time as we discover better solutions.

## Why?

JSON Schema is a frequent visitor in the backend. One schema might be used for OpenAPI Specification, request validation as well as faster response serialization. Even if you're not related to the backend you can use it to parse or validate data that came from an unreliable source.  

**rescript-json-schema** uses **rescript-struct** to build 100% typesafe schema. No need to worry about type definitions becoming unsync with generated schema. ReScript compiler will check it for you.

## Installation

Install **rescript-struct** following its [installation instruction](https://github.com/DZakh/rescript-struct#installation)

Install **rescript-json-schema**

```sh
npm install rescript-json-schema
```

Then add `rescript-json-schema` to `bs-dependencies` in your `bsconfig.json`:

```diff
{
  ...
+ "bs-dependencies": ["rescript-json-schema"]
}
```

## Usage
### Define a struct

Mostly you'll work with **rescript-struct** to describe the structure of the value. The structure contains meta information for parsing, serializing, and generating JSON Schema.

```rescript
type author = {
  id: float,
  tags: array<string>,
  isAproved: bool,
  deprecatedAge: option<int>
}

let authorStruct: S.t<author> = S.record4(
  ~fields=(
    ("Id", S.float()),
    ("Tags", S.array(S.string())),
    ("IsApproved", S.option(S.int()->S.transform(~constructor=int =>
          switch int {
          | 1 => true
          | _ => false
          }->Ok
        , ()))->S.default(false)),
    ("Age", S.deprecated(~message="A useful explanation", S.int())),
  ),
  ~constructor=((id, tags, isAproved, deprecatedAge)) =>
    {id: id, tags: tags, isAproved: isAproved, deprecatedAge: deprecatedAge}->Ok,
  (),
)
```

### Make JSON Schema

When the struct is defined it can be used to generate JSON Schema.

> I recommend hiding the conversion to JSON Schema behind abstraction and working only with structs in application code.

```rescript
JsonSchema.make(authorStruct)
```
```js
Ok({
  '$schema': 'http://json-schema.org/draft-07/schema#',
  additionalProperties: false,
  properties: {
    Age: {
      deprecated: true,
      description: 'A useful explanation',
      type: 'integer'
    },
    Id: { type: 'number' },
    IsApproved: { type: 'integer' },
    Tags: { 
      default: [],
      items: { type: 'string' },
      type: 'array'
    }
  },
  required: [ 'Id', 'IsApproved' ],
  type: 'object'
})
```

### Decoding validated data

If you have data that you're 100% sure is valid, you can construct it to ReScript value. This operation doesn't have any checks and is only used for mapping.

More about this in the **[rescript-struct](https://github.com/DZakh/rescript-struct#usage)** documentation.

```rescript
let data = %raw(`{
  "Id": 1,
  "IsApproved": 1,
  "Age": 12,
}`)

let constructResult: result<author, string> = data->S.constructWith(authorStruct)
// Equal to:
// Ok({
//   id: 1.,
//   tags: [],
//   isAproved: true,
//   deprecatedAge: Some(12),
// })
```

The JSON has capitalized field names, after decoding they are mapped to a valid ReScript structure.

## V1 Roadmap

- [ ] Add example/examples field
- [ ] Add refinements
- [ ] Add support for $refs
- [ ] Able to target Open API 3 (Swagger) specification for paths
- [ ] Add API documentation
- [ ] Wait for the **rescript-struct** V1
