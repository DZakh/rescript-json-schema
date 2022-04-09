# ReScript JSON Schema

Typesafe JSON Schema for ReScript

## Status

> **rescript-json-schema** is currently in beta. Its core API is useable right now, but you might need to pull request improvements for advanced use cases, or fixes for some bugs. Some of its APIs are not "finalized" and will have breaking changes over time as we discover better solutions.

## Why?

JSON Schema is a frequent visitor in the backend. It's used for OpenAPI Specification, and frameworks such as [Fastify](https://www.fastify.io/) use it for request and response validation. Even if you're not related to the backend you've might use it to parse or validate data that came from an unreliable source.  

With **rescript-json-schema** you'll be able to work with JSON Schema without worrying about type definition becoming unsync with defined schema. ReScript compiler will check it for you.

**rescript-json-schema** is not limited only to passing schema to a framework. It's a solid alternative for JSON decoding/encoding libraries such as:
- [Jzon](https://github.com/nkrkv/jzon)
- [bs-jsno](https://github.com/glennsl/bs-json)
- [decco](https://github.com/reasonml-labs/decco)

## Installation

```sh
npm install rescript-json-schema
```

To use the parsing feature install [Ajv](https://ajv.js.org/)

```sh
npm install ajv
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

The main entity you will use is the **struct: S.t<'value>**. It describes the structure of the value, as well as contains meta information for parsing, serializing, and converting to JSON Schema.

```rescript
type author = {id: float}
type article = {
  id: float,
  title: string,
  description: option<string>,
  tags: array<string>,
  author: author,
}

let articleStruct: S.t<article> = S.record5(
  ~fields=(
    ("Id", S.float()),
    ("Title", S.string()),
    ("Description", S.option(S.string())),
    ("Tags", S.array(S.string())),
    ("Author", S.record1(~fields=("Id", S.float()), ~constructor=id => {id: id}->Ok, ())),
  ),
  ~constructor=((id, title, description, tags, author)) => {
    id: id,
    title: title,
    description: description,
    tags: tags,
    author: author,
  }->Ok,
  ()
)
```

### Make JSON Schema

When the struct is defined it can be used to generate JSON Schema.

> I recommend hiding the conversion to JSON Schema behind a framework and working only with struct in application code.

```rescript
Js.log(JsonSchema.make(articleStruct))

// Output:
// {
//   '$schema': 'http://json-schema.org/draft-07/schema#',
//   type: 'object',
//   properties: {
//     Id: { type: 'number' },
//     Title: { type: 'string' },
//     Description: { type: 'string' },
//     Tags: { type: 'array', items: { type: 'string' } },
//     Author: {
//       type: 'object',
//       properties: {
//         Id: { type: 'number' }
//       }
//     }
//   },
//   required: [ 'Id', 'Title', 'Tags', 'Author' ]
// }
```

### Decoding validated data

If you have data that you're 100% sure is valid, you can construct it to ReScript value. This operation doesn't have any checks and is only needed for mapping.

```rescript
let data = Js.Json.parseExn(`{
  "Id": 34,
  "Title": "Hello World",
  "Tags": ["news", "features"],
  "Author": {
    "Id": 1
  }
}`)

let constructResult: result<article, string> = articleStruct->S.construct(data)
// or
let constructResult: result<article, string> = data->S.constructWith(articleStruct)
```

The JSON has capitalized field names, after decoding they are mapped to a valid ReScript structure.

### Parsing data

The `constructor` is suitable for cases when the data is already valid, or else you'll get a runtime error or invalid state.  
To work with unknown data use built-in [Ajv](https://ajv.js.org/) bindings.

```rescript
let data = Js.Json.parseExn(`{
  "Id": 34,
  "Title": "Hello World",
  "Tags": ["news", "features"],
  "Author": {
    "Id": 1
  }
}`)

let ajv = Ajv.make()
let articleValidator = ajv->Ajv.Validator.make(articleStruct)
let articleParseResult: result<article, string> = articleValidator->Ajv.Validator.parse(data)
```

## Custom structs

### Record

The package has built-in records up to 10 fields. If you have a record with more than 10 fields, you can create a custom struct factory for any number of fields.

```rescript
let record2: (
  ~fields: (S.field<'v1>, S.field<'v2>),
  ~constructor: (('v1, 'v2)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2), string>=?,
  unit,
) => S.t<'value> = S.Record.factory
```

> The package guts are not typesafe, so you should properly annotate the struct factory interface.

## V1 Roadmap

- [x] Add Custom struct
- [ ] Return better errors
- [x] Add Dict struct
- [ ] Add Literal struct
- [ ] Add Union struct
- [ ] Add Default modifier
- [ ] Add Unknown struct
- [ ] Add Never struct
- [ ] Add Object struct
- [x] Add ability to mixin raw schema
- [x] Add destructing
- [x] Remove Fluent JSON Schema from dependencies
- [ ] Add better support for OpenAPI
  - [x] Add description field 
  - [ ] Add example/examples field 
  - [ ] Add deprecated field
