open Ava

test("Schema of bool struct", t => {
  let struct = S.bool

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}`)),
    (),
  )
})

test("Schema of string struct", t => {
  let struct = S.string

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`)),
    (),
  )
})

test("Schema of string struct with Email refinement", t => {
  let struct = S.string->S.String.email

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "email"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Url refinement", t => {
  let struct = S.string->S.String.url

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uri"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Datetime refinement", t => {
  let struct = S.string->S.String.datetime

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }`),
    ),
    (),
  )
})

test("Schema of string struct uses the last refinement for format", t => {
  let struct = S.string->S.String.email->S.String.datetime

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Cuid refinement", t => {
  let struct = S.string->S.String.cuid

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Uuid refinement", t => {
  let struct = S.string->S.String.uuid

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uuid"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Pattern refinement", t => {
  let struct = S.string->S.String.pattern(%re("/abc/g"))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "pattern": "/abc/g"
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Min refinement", t => {
  let struct = S.string->S.String.min(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Max refinement", t => {
  let struct = S.string->S.String.max(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "maxLength": 1
      }`),
    ),
    (),
  )
})

test("Schema of string struct with Length refinement", t => {
  let struct = S.string->S.String.length(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 1
      }`),
    ),
    (),
  )
})

test("Schema of string struct with both Min and Max refinements", t => {
  let struct = S.string->S.String.min(1)->S.String.max(4)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 4
      }`),
    ),
    (),
  )
})

test("Schema of int struct", t => {
  let struct = S.int

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}`)),
    (),
  )
})

test("Schema of int struct with Min refinement", t => {
  let struct = S.int->S.Int.min(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "minimum": 1
      }`),
    ),
    (),
  )
})

test("Schema of int struct with Max refinement", t => {
  let struct = S.int->S.Int.max(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "maximum": 1
      }`),
    ),
    (),
  )
})

test("Schema of int struct with Port refinement", t => {
  let struct = S.int->S.Int.port

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer"
      }`),
    ),
    (),
  )
})

test("Schema of float struct", t => {
  let struct = S.float

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}`)),
    (),
  )
})

test("Schema of float struct with Min refinement", t => {
  let struct = S.float->S.Float.min(1.)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "minimum": 1
      }`),
    ),
    (),
  )
})

test("Schema of float struct with Max refinement", t => {
  let struct = S.float->S.Float.max(1.)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "maximum": 1
      }`),
    ),
    (),
  )
})

test("Schema of Null struct", t => {
  let struct = S.null(S.float)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [{"type": "number"}, {"type": "null"}]
      }`),
    ),
    (),
  )
})

test("Schema of Never struct", t => {
  let struct = S.never

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "not": {}
      }`),
    ),
    (),
  )
})

test("Schema of Bool Literal struct", t => {
  let struct = S.literal(false)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "const": false
      }`),
    ),
    (),
  )
})

test("Schema of String Literal struct", t => {
  let struct = S.literal("Hello World!")

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "const": "Hello World!"
      }`),
    ),
    (),
  )
})

test("Schema of Int Literal struct", t => {
  let struct = S.literal(123)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "const": 123
      }`),
    ),
    (),
  )
})

test("Schema of Float Literal struct", t => {
  let struct = S.literal(-123.456)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "const": -123.456
      }`),
    ),
    (),
  )
})

test("Schema of EmptyNull Literal struct", t => {
  let struct = S.literal(%raw(`null`))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "null",
      }`),
    ),
    (),
  )
})

test("Schema of EmptyOption Literal struct isn't supported", t => {
  let struct = S.literal(%raw(`undefined`))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The Literal(undefined) struct is not supported`),
    (),
  )
})

test("Schema of NaN Literal struct isn't supported", t => {
  let struct = S.literal(%raw(`NaN`))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The Literal(NaN) struct is not supported`),
    (),
  )
})

test("Schema of tuple struct", t => {
  let struct = S.tuple2(S.string, S.bool)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "minItems": 2,
        "maxItems": 2,
        "items": [{"type": "string"}, {"type": "boolean"}],
      }`),
    ),
    (),
  )
})

test("Schema of union struct", t => {
  let struct = S.union([S.literal("Yes"), S.literal("No")])

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [
          {
            const: 'Yes',
            type: 'string'
          },
          {
            const: 'No',
            type: 'string'
          }
        ]
      }`),
    ),
    (),
  )
})

test("Schema of strings array struct", t => {
  let struct = S.array(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
      }`),
    ),
    (),
  )
})

test("Schema of array struct with Min refinement", t => {
  let struct = S.array(S.string)->S.Array.min(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1
      }`),
    ),
    (),
  )
})

test("Schema of array struct with Max refinement", t => {
  let struct = S.array(S.string)->S.Array.max(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "maxItems": 1
      }`),
    ),
    (),
  )
})

test("Schema of array struct with Length refinement", t => {
  let struct = S.array(S.string)->S.Array.length(1)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1,
        "maxItems": 1
      }`),
    ),
    (),
  )
})

test("Schema of strings dict struct", t => {
  let struct = S.dict(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": {"type": "string"},
      }`),
    ),
    (),
  )
})

test("Schema of object struct with one string field", t => {
  let struct = S.object(s => s.field("field", S.string))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with one string discriminant", t => {
  let struct = S.object(s => {
    ignore(s.field("field", S.string))
    ()
  })

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with Strip unknownKeys strategy allows additionalProperties", t => {
  let struct = S.object(s => s.field("field", S.string))->S.Object.strip

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test(
  "Schema of object struct with Strict unknownKeys strategy disallows additionalProperties",
  t => {
    let struct = S.object(s => s.field("field", S.string))->S.Object.strict

    t->Assert.deepEqual(
      JSONSchema.make(struct),
      Ok(
        %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": false,
      }`),
      ),
      (),
    )
  },
)

test("Schema of object struct with one optional string field", t => {
  let struct = S.object(s => s.field("optionalField", S.option(S.string)))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string"}},
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with one deprecated string field", t => {
  let struct = S.object(s => s.field("field", S.string->S.deprecate("Use another field")))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "required": [ "field" ],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Deprecated message overrides previous description", t => {
  let struct = S.object(s =>
    s.field("field", S.string->S.describe("Previous description")->S.deprecate("Use another field"))
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "required": [ 'field' ],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with nested object", t => {
  let struct = S.object(s =>
    s.field("objectWithOneStringField", S.object(s => s.field("Field", S.string)))
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "objectWithOneStringField": {
            "type": "object",
            "properties": {"Field": {"type": "string"}},
            "required": ["Field"],
            "additionalProperties": true,
          },
        },
        "required": ["objectWithOneStringField"],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with one optional and one required string field", t => {
  let struct = S.object(s => (
    s.field("field", S.string),
    s.field("optionalField", S.option(S.string)),
  ))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {
            "type": "string",
          },
          "optionalField": {"type": "string"},
        },
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Make JSONSchema throws error with optional root type", t => {
  let struct = S.option(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported at root",
    ),
    (),
  )
})

test("Make JSONSchema throws error with object field wrapped in option multiple times", t => {
  let struct = S.object(s => s.field("optionalOptionalField", S.option(S.option(S.string))))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["optionalOptionalField"]. Reason: Optional struct is not supported inside the Option struct`),
    (),
  )
})

test("Primitive struct schema with description", t => {
  let struct = S.bool->S.describe("A primitive struct")

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "A primitive struct",
      }`),
    ),
    (),
  )
})

test("Transformed struct schema with default fails when destruction failed", t => {
  let struct = S.object(s =>
    s.field(
      "field",
      S.option(
        S.bool->S.transform(
          _ => {
            parser: bool => {
              switch bool {
              | true => "true"
              | false => ""
              }
            },
          },
        ),
      )->S.Option.getOr("true"),
    )
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["field"]. Reason: Couldn't destruct default value. Error: Failed serializing at root. Reason: The S.transform serializer is missing`),
    (),
  )
})

test("Transformed struct schema uses default with correct type", t => {
  let struct = S.object(s =>
    s.field(
      "field",
      S.option(
        S.bool->S.transform(
          _ => {
            parser: bool => {
              switch bool {
              | true => "true"
              | false => ""
              }
            },
            serializer: string => {
              switch string {
              | "true" => true
              | _ => false
              }
            },
          },
        ),
      )->S.Option.getOrWith(() => "true"),
    )
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "additionalProperties": true,
        "properties": {"field": {"default": true, "type": "boolean"}},
        "type": "object",
      }`),
    ),
    (),
  )
})

test("Primitive struct schema with additional raw schema", t => {
  let struct = S.bool->JSONSchema.extend({description: "foo"})

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "foo",
      }`),
    ),
    (),
  )
})

test("Multiple additional raw schemas are merged together", t => {
  let struct =
    S.bool
    ->JSONSchema.extend({"nullable": true}->Obj.magic)
    ->JSONSchema.extend({"deprecated": true}->Obj.magic)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "deprecated": true,
        "nullable": true,
      }`),
    ),
    (),
  )
})

test("Additional raw schema works with optional fields", t => {
  let struct = S.object(s =>
    s.field("optionalField", S.option(S.string)->JSONSchema.extend({"nullable": true}->Obj.magic))
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"nullable": true, "type": "string"},
        },
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Unknown struct doesn't affect final schema", t => {
  let struct = S.unknown

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
      }`),
    ),
    (),
  )
})

test("JSON struct doesn't affect final schema", t => {
  let struct = S.json

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
      }`),
    ),
    (),
  )
})

test("Fails to create schema for structs with optional items", t => {
  t->Assert.deepEqual(
    JSONSchema.make(S.dict(S.option(S.string))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Dict(Option(String)) item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.array(S.option(S.string))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Array(Option(String)) item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.union([S.option(S.string), S.null(S.string)])),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Union(Option(String), Null(String)) item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.tuple1(S.option(S.string))),
    Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional struct is not supported as Tuple(Option(String)) item`),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.tuple1(S.array(S.option(S.string)))),
    Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional struct is not supported as Array(Option(String)) item`),
    (),
  )
})

module Example = {
  type rating =
    | @as("G") GeneralAudiences
    | @as("PG") ParentalGuidanceSuggested
    | @as("PG13") ParentalStronglyCautioned
    | @as("R") Restricted
  type film = {
    id: float,
    title: string,
    tags: array<string>,
    rating: rating,
    deprecatedAgeRestriction: option<int>,
  }

  test("Example", t => {
    let filmStruct = S.object(s => {
      id: s.field("Id", S.float),
      title: s.field("Title", S.string),
      tags: s.fieldOr("Tags", S.array(S.string), []),
      rating: s.field(
        "Rating",
        S.union([
          S.literal(GeneralAudiences),
          S.literal(ParentalGuidanceSuggested),
          S.literal(ParentalStronglyCautioned),
          S.literal(Restricted),
        ]),
      ),
      deprecatedAgeRestriction: s.field("Age", S.option(S.int)->S.deprecate("Use rating instead")),
    })

    t->Assert.deepEqual(
      JSONSchema.make(filmStruct),
      Ok(
        %raw(`{
          $schema: "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            Id: { type: "number" },
            Title: { type: "string" },
            Tags: { items: { type: "string" }, type: "array", default: [] },
            Rating: {
              anyOf: [
                { type: "string", const: "G" },
                { type: "string", const: "PG" },
                { type: "string", const: "PG13" },
                { type: "string", const: "R" },
              ],
            },
            Age: {
              type: "integer",
              deprecated: true,
              description: "Use rating instead",
            },
          },
          additionalProperties: true,
          required: ["Id", "Title", "Rating"],
        }`),
      ),
      (),
    )
  })
}
