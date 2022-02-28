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

let articleStruct: S.t<article> = {
  open S
  record5(
    ~fields=(
      ("Id", float),
      ("Title", string),
      ("Description", option(string)),
      ("Tags", array(string)),
      ("Author", record1(~fields=("Id", float), ~constructor=id => {id: id})),
    ),
    ~constructor=((id, title, description, tags, author)) => {
      id: id,
      title: title,
      description: description,
      tags: tags,
      author: author,
    },
  )
}
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
//         Id: { type: 'number', '$id': undefined }
//       }
//     }
//   },
//   required: [ 'Id', 'Title', 'Tags', 'Author' ]
// }
```

### Decoding validated data

If you have data that you're 100% sure is valid, you can decode it to ReScript structure. This operation doesn't have any checks and is only needed for mapping.

```rescript
let data = Js.Json.parseExn(`{
  "Id": 34,
  "Title": "Hello World",
  "Tags": ["news", "features"],
  "Author": {
    "Id": 1
  }
}`)

let article: article = articleStruct->S.decode(data)
// or
let article: article = data->S.decodeWith(articleStruct)
```

The JSON has capitalized field names, after decoding they are mapped to a valid ReScript structure.

### Parsing data

The decoding is suitable for cases when the data is already valid, or else you'll get runtime error or invalid state.  
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
let articleParseResult: result<article, unit> = articleValidator->Ajv.Validator.parse(data)
```

## V1 Roadmap

- [ ] Add Custom struct
- [ ] Add more detailed errors
- [ ] Add Literal struct
- [ ] Add Union struct
- [ ] Add default modifier
- [ ] Add nullable modifier
- [ ] Add unknown struct
- [ ] Add never struct
- [ ] Add ability to mixin raw schema
- [ ] Add encoding
- [v] Remove Fluent JSON Schema from dependencies
- [ ] Make unknown an abstract type
- [ ] Add better support for OpenAPI
  - [ ] Add description field 
  - [ ] Add example/examples field 
  - [ ] Add deprecated field
