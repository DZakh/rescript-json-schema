# ReScript JSON Schema

Typesafe JSON Schema for ReScript

**rescript-json-schema** uses **rescript-struct** to build 100% typesafe schema. No need to worry about type definitions becoming unsync with generated schema. ReScript compiler will check it for you.

## Status

> **rescript-json-schema** is currently in beta. Its core API is useable right now, but you might need to pull request improvements for advanced use cases, or fixes for some bugs. Some of its APIs are not "finalized" and will have breaking changes over time as we discover better solutions.

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

```rescript
let authorStruct = S.record4(.
  ("Id", S.float()),
  ("Tags", S.option(S.array(S.string()))),
  (
    "IsApproved",
    S.union([
      S.literalVariant(String("Yes"), true),
      S.literalVariant(String("No"), false),
    ]),
  ),
  ("Age", S.deprecated(~message="A useful explanation", S.int())),
)

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
    IsApproved: { 
      anyOf: [
        {
          const: 'Yes',
          type: 'string'
        },
        {
          const: 'No',
          type: 'string'
        }]
      },
    Tags: { 
      items: { type: 'string' },
      type: 'array'
    }
  },
  required: ['Id', 'IsApproved'],
  type: 'object'
})
```

Mostly you'll work with **rescript-struct** to describe the structure of the value. The structure contains meta information for parsing, serializing, and generating JSON Schema.

## V1 Roadmap

- [ ] Add example/examples field
- [ ] Add refinements
- [ ] Add support for $refs
- [ ] Able to target Open API 3 (Swagger) specification for paths
- [ ] Add API documentation
- [ ] Wait for the **rescript-struct** V1
