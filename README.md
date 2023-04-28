[![CI](https://github.com/DZakh/rescript-json-schema/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/rescript-json-schema/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/DZakh/rescript-json-schema/branch/main/graph/badge.svg?token=40G6YKKD6J)](https://codecov.io/gh/DZakh/rescript-json-schema)
[![npm](https://img.shields.io/npm/dm/rescript-json-schema)](https://www.npmjs.com/package/rescript-json-schema)

# ReScript JSON Schema

Typesafe JSON Schema for ReScript

- Provides ReScript types to work with [JSON schema](https://json-schema.org/)
- Converts [**rescript-struct**](https://github.com/DZakh/rescript-struct) into JSON schemas
- Converts JSON schemas to [**rescript-struct**](https://github.com/DZakh/rescript-struct)

## Install

```sh
npm install rescript-json-schema rescript-struct
```

Then add `rescript-json-schema` and `rescript-struct` to `bs-dependencies` in your `bsconfig.json`:

```diff
{
  ...
+ "bs-dependencies": ["rescript-json-schema", "rescript-struct"]
+ "bsc-flags": ["-open RescriptStruct"],
}
```

## Create JSON schemas with type safety

One of the library's main features is the **rescript-struct**, which provides a way to describe the structure of a value. This structure contains meta information used for parsing, serializing, and generating JSON Schema. When working with the library, you will mostly interact with **rescript-struct** to define the structure of the values you are working with.

For example, if you have the following struct:

```rescript
type author = {
  id: float,
  tags: array<string>,
  isAproved: bool,
  deprecatedAge: option<int>,
}

let authorStruct = S.object(o => {
  id: o->S.field("Id", S.float()),
  tags: o->S.field("Tags", S.option(S.array(S.string()))->S.default(() => [])),
  isAproved: o->S.field(
    "IsApproved",
    S.union([S.literalVariant(String("Yes"), true), S.literalVariant(String("No"), false)]),
  ),
  deprecatedAge: o->S.field("Age", S.int()->S.deprecate("Will be removed in APIv2")),
})
```

You can use it to generate JSON Schema for the value it describes:

```rescript
JSONSchema.make(authorStruct)
```

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "Age": {
      "deprecated": true,
      "description": "Will be removed in APIv2",
      "type": "integer"
    },
    "Id": { "type": "number" },
    "IsApproved": {
      "anyOf": [
        {
          "const": "Yes",
          "type": "string"
        },
        {
          "const": "No",
          "type": "string"
        }
      ]
    },
    "Tags": {
      "items": { "type": "string" },
      "type": "array"
    }
  },
  "required": ["Id", "IsApproved"],
  "additionalProperties": true
}
```

## Create **rescript-struct** from JSON schema

### Online

![ReScript JSON Schema Online](assets/online-preview.png)

[Just paste your JSON schemas here!](https://dzakh.github.io/rescript-json-schema/)
