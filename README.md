[![CI](https://github.com/DZakh/rescript-json-schema/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/rescript-json-schema/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/DZakh/rescript-json-schema/branch/main/graph/badge.svg?token=40G6YKKD6J)](https://codecov.io/gh/DZakh/rescript-json-schema)
[![npm](https://img.shields.io/npm/dm/rescript-json-schema)](https://www.npmjs.com/package/rescript-json-schema)

# ReScript JSON Schema

Typesafe JSON Schema for ReScript

**rescript-json-schema** is a library that generates type-safe JSON schemas using [**rescript-struct**](https://github.com/DZakh/rescript-struct). This ensures that your schemas are always in sync with your ReScript code and are fully type-checked and valid.

## Install

```sh
npm install rescript-json-schema rescript-struct
```

Then add `rescript-json-schema` and `rescript-struct` to `bs-dependencies` in your `bsconfig.json`:

```diff
{
  ...
+ "bs-dependencies": ["rescript-json-schema", "rescript-struct"]
+ "bsc-flags": ["-open ReScriptStruct"],
}
```

> 🧠 You need to have rescript >10.1.0

## Basic usage

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
  tags: o->S.field("Tags", S.option(S.array(S.string()))->S.defaulted([])),
  isAproved: o->S.field(
    "IsApproved",
    S.union([S.literalVariant(String("Yes"), true), S.literalVariant(String("No"), false)]),
  ),
  deprecatedAge: o->S.field(
    "Age",
    S.int()->S.deprecated(~message="Will be removed in APIv2", ()),
  ),
})
```

You can use it to generate JSON Schema for the value it describes:

```rescript
JsonSchema.make(authorStruct)
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
