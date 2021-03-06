open Ava

test("Schema of bool struct", t => {
  let struct = S.bool()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}`)),
    (),
  )
})

test("Schema of string struct", t => {
  let struct = S.string()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`)),
    (),
  )
})

test("Schema of int struct", t => {
  let struct = S.int()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}`)),
    (),
  )
})

test("Schema of float struct", t => {
  let struct = S.float()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}`)),
    (),
  )
})

test("Schema of Null struct", t => {
  let struct = S.null(S.float())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.never()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(Bool(false))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(String("Hello World!"))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(Int(123))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(Float(-123.456))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(EmptyNull)

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.literal(EmptyOption)

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The EmptyOption Literal struct is not supported`),
    (),
  )
})

test("Schema of NaN Literal struct isn't supported", t => {
  let struct = S.literal(NaN)

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The NaN Literal struct is not supported`),
    (),
  )
})

test("Schema of tuple struct", t => {
  let struct = S.tuple2(. S.string(), S.bool())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.union([
    S.literalVariant(String("Yes"), true),
    S.literalVariant(String("No"), false),
  ])

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.array(S.string())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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

test("Schema of strings dict struct", t => {
  let struct = S.dict(S.string())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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

test("Schema of record struct with one string field", t => {
  let struct = S.record1(. ("field", S.string()))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
})

test("Schema of record struct with Strip unknownKeys strategy allows additionalProperties", t => {
  let struct = S.record1(. ("field", S.string()))->S.Record.strip

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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

test("Schema of record struct with one optional string field", t => {
  let struct = S.record1(. ("optionalField", S.option(S.string())))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string"}},
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Schema of record struct with one deprecated string field", t => {
  let struct = S.record1(. ("optionalField", S.deprecated(S.string())))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string", "deprecated": true}},
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Schema of record struct with one deprecated string field and message", t => {
  let struct = S.record1(. (
    "optionalField",
    S.deprecated(~message="Use another field", S.string()),
  ))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Deprecated message overrides previous description", t => {
  let struct = S.record1(. (
    "optionalField",
    S.deprecated(
      ~message="Use another field",
      S.string()->JsonSchema.description("Previous description"),
    ),
  ))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Schema of record struct with nested record", t => {
  let struct = S.record1(. ("recordWithOneStringField", S.record1(. ("Field", S.string()))))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "recordWithOneStringField": {
            "type": "object",
            "properties": {"Field": {"type": "string"}},
            "required": ["Field"],
            "additionalProperties": false,
          },
        },
        "required": ["recordWithOneStringField"],
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Schema of record struct with one optional and one required string field", t => {
  let struct = S.record2(. ("field", S.string()), ("optionalField", S.option(S.string())))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Make JsonSchema throws error with optional root type", t => {
  let struct = S.option(S.string())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported at root",
    ),
    (),
  )
})

test("Make JsonSchema throws error with record field wrapped in option multiple times", t => {
  let struct = S.record1(. ("optionalOptionalField", S.option(S.option(S.string()))))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["optionalOptionalField"]. Reason: Optional struct is not supported inside the Option struct`),
    (),
  )
})

test("Primitive struct schema with description", t => {
  let struct = S.bool()->JsonSchema.description("A primitive struct")

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.record1(. ("field", S.option(S.bool()->S.transform(~parser=bool => {
        switch bool {
        | true => "true"
        | false => ""
        }->Ok
      }, ()))->S.default("true")))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["field"]. Reason: Couldn't destruct default value. Error: [ReScript Struct] Failed serializing at root. Reason: Struct serializer is missing`),
    (),
  )
})

test("Transformed struct schema uses default with correct type", t => {
  let struct = S.record1(. (
    "field",
    S.option(
      S.bool()->S.transform(
        ~parser=bool => {
          switch bool {
          | true => "true"
          | false => ""
          }->Ok
        },
        ~serializer=string => {
          switch string {
          | "true" => true
          | _ => false
          }->Ok
        },
        (),
      ),
    )->S.default("true"),
  ))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "additionalProperties": false,
        "properties": {"field": {"default": true, "type": "boolean"}},
        "type": "object",
      }`),
    ),
    (),
  )
})

test("Primitive struct schema with additional raw schema", t => {
  let struct = S.bool()->JsonSchema.raw({"nullable": true})

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "nullable": true,
      }`),
    ),
    (),
  )
})

test("Multiple additional raw schemas are merged together", t => {
  let struct = S.bool()->JsonSchema.raw({"nullable": true})->JsonSchema.raw({"deprecated": true})

  t->Assert.deepEqual(
    JsonSchema.make(struct),
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
  let struct = S.record1(. (
    "optionalField",
    S.option(S.string())->JsonSchema.raw({"nullable": true}),
  ))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"nullable": true, "type": "string"},
        },
        "additionalProperties": false,
      }`),
    ),
    (),
  )
})

test("Unknown struct doesn't affect final schema", t => {
  let struct = S.unknown()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
      }`),
    ),
    (),
  )
})

module Example = {
  test("Example", t => {
    let authorStruct = S.record4(.
      ("Id", S.float()),
      ("Tags", S.option(S.array(S.string()))->S.default([])),
      (
        "IsApproved",
        S.union([S.literalVariant(String("Yes"), true), S.literalVariant(String("No"), false)]),
      ),
      ("Age", S.deprecated(~message="A useful explanation", S.int())),
    )

    t->Assert.deepEqual(
      JsonSchema.make(authorStruct),
      Ok(
        %raw(`{
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
                }
              ]
             },
            Tags: { 
              default: [],
              items: { type: 'string' },
              type: 'array'
            }
          },
          required: [ 'Id', 'IsApproved' ],
          type: 'object'
        }`),
      ),
      (),
    )
  })
}
