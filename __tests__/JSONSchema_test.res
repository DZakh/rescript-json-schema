open Ava

test("Schema of bool schema", t => {
  let schema = S.bool

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}`)),
  )
})

test("Schema of string schema", t => {
  let schema = S.string

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`)),
  )
})

test("Schema of string schema with Email refinement", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "email"
      }`),
    ),
  )
})

test("Schema of string schema with Url refinement", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uri"
      }`),
    ),
  )
})

test("Schema of string schema with Datetime refinement", t => {
  let schema = S.string->S.datetime

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }`),
    ),
  )
})

test("Schema of string schema uses the last refinement for format", t => {
  let schema = S.string->S.email->S.datetime

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }`),
    ),
  )
})

test("Schema of string schema with Cuid refinement", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string"
      }`),
    ),
  )
})

test("Schema of string schema with Uuid refinement", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uuid"
      }`),
    ),
  )
})

test("Schema of string schema with Pattern refinement", t => {
  let schema = S.string->S.pattern(%re("/abc/g"))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "pattern": "/abc/g"
      }`),
    ),
  )
})

test("Schema of string schema with Min refinement", t => {
  let schema = S.string->S.stringMinLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1
      }`),
    ),
  )
})

test("Schema of string schema with Max refinement", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "maxLength": 1
      }`),
    ),
  )
})

test("Schema of string schema with Length refinement", t => {
  let schema = S.string->S.stringLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 1
      }`),
    ),
  )
})

test("Schema of string schema with both Min and Max refinements", t => {
  let schema = S.string->S.stringMinLength(1)->S.stringMaxLength(4)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 4
      }`),
    ),
  )
})

test("Schema of int schema", t => {
  let schema = S.int

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}`)),
  )
})

test("Schema of int schema with Min refinement", t => {
  let schema = S.int->S.intMin(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "minimum": 1
      }`),
    ),
  )
})

test("Schema of int schema with Max refinement", t => {
  let schema = S.int->S.intMax(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "maximum": 1
      }`),
    ),
  )
})

test("Schema of int schema with Port refinement", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer"
      }`),
    ),
  )
})

test("Schema of float schema", t => {
  let schema = S.float

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}`)),
  )
})

test("Schema of float schema with Min refinement", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "minimum": 1
      }`),
    ),
  )
})

test("Schema of float schema with Max refinement", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "maximum": 1
      }`),
    ),
  )
})

test("Schema of Null schema", t => {
  let schema = S.null(S.float)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [{"type": "number"}, {"type": "null"}]
      }`),
    ),
  )
})

test("Schema of Never schema", t => {
  let schema = S.never

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "not": {}
      }`),
    ),
  )
})

test("Schema of Bool Literal schema", t => {
  let schema = S.literal(false)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "const": false
      }`),
    ),
  )
})

test("Schema of String Literal schema", t => {
  let schema = S.literal("Hello World!")

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "const": "Hello World!"
      }`),
    ),
  )
})

test("Schema of Int Literal schema", t => {
  let schema = S.literal(123)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "const": 123
      }`),
    ),
  )
})

test("Schema of Float Literal schema", t => {
  let schema = S.literal(-123.456)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "const": -123.456
      }`),
    ),
  )
})

test("Schema of EmptyNull Literal schema", t => {
  let schema = S.literal(%raw(`null`))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "null",
      }`),
    ),
  )
})

test("Schema of EmptyOption Literal schema isn't supported", t => {
  let schema = S.literal(%raw(`undefined`))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The undefined schema is not supported`),
  )
})

test("Schema of NaN Literal schema isn't supported", t => {
  let schema = S.literal(%raw(`NaN`))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The NaN schema is not supported`),
  )
})

test("Schema of tuple schema", t => {
  let schema = S.tuple2(S.string, S.bool)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "minItems": 2,
        "maxItems": 2,
        "items": [{"type": "string"}, {"type": "boolean"}],
      }`),
    ),
  )
})

test("Schema of union schema", t => {
  let schema = S.union([S.literal("Yes"), S.literal("No")])

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Schema of strings array schema", t => {
  let schema = S.array(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
      }`),
    ),
  )
})

test("Schema of array schema with Min refinement", t => {
  let schema = S.array(S.string)->S.arrayMinLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1
      }`),
    ),
  )
})

test("Schema of array schema with Max refinement", t => {
  let schema = S.array(S.string)->S.arrayMaxLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "maxItems": 1
      }`),
    ),
  )
})

test("Schema of array schema with Length refinement", t => {
  let schema = S.array(S.string)->S.arrayLength(1)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1,
        "maxItems": 1
      }`),
    ),
  )
})

test("Schema of strings dict schema", t => {
  let schema = S.dict(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": {"type": "string"},
      }`),
    ),
  )
})

test("Schema of object schema with one string field", t => {
  let schema = S.object(s => s.field("field", S.string))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
  )
})

test("Schema of object schema with one string discriminant", t => {
  let schema = S.object(s => {
    ignore(s.field("field", S.string))
    ()
  })

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
  )
})

test("Schema of object schema with Strip unknownKeys strategy allows additionalProperties", t => {
  let schema = S.object(s => s.field("field", S.string))->S.strip

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }`),
    ),
  )
})

test(
  "Schema of object schema with Strict unknownKeys strategy disallows additionalProperties",
  t => {
    let schema = S.object(s => s.field("field", S.string))->S.strict

    t->Assert.deepEqual(
      JSONSchema.make(schema),
      Ok(
        %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": false,
      }`),
      ),
    )
  },
)

test("Schema of object schema with one optional string field", t => {
  let schema = S.object(s => s.field("optionalField", S.option(S.string)))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string"}},
        "additionalProperties": true,
      }`),
    ),
  )
})

test("Schema of object schema with one deprecated string field", t => {
  let schema = S.object(s => s.field("field", S.string->S.deprecate("Use another field")))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Deprecated message overrides previous description", t => {
  let schema = S.object(s =>
    s.field("field", S.string->S.describe("Previous description")->S.deprecate("Use another field"))
  )

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Schema of object schema with nested object", t => {
  let schema = S.object(s =>
    s.field("objectWithOneStringField", S.object(s => s.field("Field", S.string)))
  )

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Schema of object schema with one optional and one required string field", t => {
  let schema = S.object(s => (
    s.field("field", S.string),
    s.field("optionalField", S.option(S.string)),
  ))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Make JSONSchema throws error with optional root type", t => {
  let schema = S.option(S.string)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported at root",
    ),
  )
})

test("Make JSONSchema throws error with object field wrapped in option multiple times", t => {
  let schema = S.object(s => s.field("optionalOptionalField", S.option(S.option(S.string))))

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Error(`[ReScript JSON Schema] Failed converting at ["optionalOptionalField"]. Reason: Optional schema is not supported inside the Option schema`),
  )
})

test("Primitive schema schema with description", t => {
  let schema = S.bool->S.describe("A primitive schema")

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "A primitive schema",
      }`),
    ),
  )
})

test("Transformed schema schema with default fails when destruction failed", t => {
  let schema = S.object(s =>
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
    JSONSchema.make(schema),
    Error(`[ReScript JSON Schema] Failed converting at ["field"]. Reason: Couldn't destruct default value. Error: Failed reverse converting to JSON at root. Reason: The S.transform serializer is missing`),
  )
})

test("Transformed schema schema uses default with correct type", t => {
  let schema = S.object(s =>
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
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "additionalProperties": true,
        "properties": {"field": {"default": true, "type": "boolean"}},
        "type": "object",
      }`),
    ),
  )
})

test("Primitive schema schema with additional raw schema", t => {
  let schema = S.bool->JSONSchema.extend({description: "foo"})

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "foo",
      }`),
    ),
  )
})

test("Primitive schema with an example", t => {
  let schema = S.bool->JSONSchema.example(true)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "examples": [true],
      }`),
    ),
  )
})

test("Transformed schema with an example", t => {
  let schema = S.null(S.bool)->JSONSchema.example(None)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [{"type": "boolean"}, {"type": "null"}],
        "examples": [null],
      }`),
    ),
  )
})

test("Multiple examples", t => {
  let schema = S.string->JSONSchema.example("Hi")->JSONSchema.example("It's me")

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "examples": ["Hi", "It's me"],
      }`),
    ),
  )
})

test("Multiple additional raw schemas are merged together", t => {
  let schema =
    S.bool
    ->JSONSchema.extend({"nullable": true}->Obj.magic)
    ->JSONSchema.extend({"deprecated": true}->Obj.magic)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "deprecated": true,
        "nullable": true,
      }`),
    ),
  )
})

test("Additional raw schema works with optional fields", t => {
  let schema = S.object(s =>
    s.field("optionalField", S.option(S.string)->JSONSchema.extend({"nullable": true}->Obj.magic))
  )

  t->Assert.deepEqual(
    JSONSchema.make(schema),
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
  )
})

test("Unknown schema doesn't affect final schema", t => {
  let schema = S.unknown

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
      }`),
    ),
  )
})

test("JSON schema doesn't affect final schema", t => {
  let schema = S.json(~validate=false)

  t->Assert.deepEqual(
    JSONSchema.make(schema),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
      }`),
    ),
  )
})

test("Fails to create schema for schemas with optional items", t => {
  t->Assert.deepEqual(
    JSONSchema.make(S.dict(S.option(S.string))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as dict<string | undefined> item",
    ),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.array(S.option(S.string))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as array<string | undefined> item",
    ),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.union([S.option(S.string), S.null(S.string)])),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as string | undefined | string | null item",
    ),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.tuple1(S.option(S.string))),
    Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional schema is not supported as [string | undefined] item`),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.tuple1(S.array(S.option(S.string)))),
    Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional schema is not supported as array<string | undefined> item`),
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
    let filmSchema = S.object(s => {
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
      JSONSchema.make(filmSchema),
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
    )
  })
}
