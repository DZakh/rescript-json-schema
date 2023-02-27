open Ava

test("Schema of bool struct", t => {
  let struct = S.bool()

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}`)),
    (),
  )
})

test("Schema of string struct", t => {
  let struct = S.string()

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`)),
    (),
  )
})

test("Schema of int struct", t => {
  let struct = S.int()

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}`)),
    (),
  )
})

test("Schema of float struct", t => {
  let struct = S.float()

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(%raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}`)),
    (),
  )
})

test("Schema of Null struct", t => {
  let struct = S.null(S.float())

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
  let struct = S.never()

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
  let struct = S.literal(Bool(false))

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
  let struct = S.literal(String("Hello World!"))

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
  let struct = S.literal(Int(123))

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
  let struct = S.literal(Float(-123.456))

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
  let struct = S.literal(EmptyNull)

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
  let struct = S.literal(EmptyOption)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The EmptyOption Literal (undefined) struct is not supported`),
    (),
  )
})

test("Schema of NaN Literal struct isn't supported", t => {
  let struct = S.literal(NaN)

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at root. Reason: The NaN Literal (NaN) struct is not supported`),
    (),
  )
})

test("Schema of tuple struct", t => {
  let struct = S.tuple2(. S.string(), S.bool())

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
  let struct = S.union([
    S.literalVariant(String("Yes"), true),
    S.literalVariant(String("No"), false),
  ])

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
  let struct = S.array(S.string())

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

test("Schema of strings dict struct", t => {
  let struct = S.dict(S.string())

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
  let struct = S.object(o => o->S.field("field", S.string()))

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
  let struct = S.object(o => o->S.discriminant("field", S.string()))

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
  let struct = S.object(o => o->S.field("field", S.string()))->S.Object.strip

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
    let struct = S.object(o => o->S.field("field", S.string()))->S.Object.strict

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
  let struct = S.object(o => o->S.field("optionalField", S.option(S.string())))

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
  let struct = S.object(o => o->S.field("optionalField", S.string()->S.deprecated()))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string", "deprecated": true}},
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with one deprecated string field and message", t => {
  let struct = S.object(o =>
    o->S.field("optionalField", S.string()->S.deprecated(~message="Use another field", ()))
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Deprecated message overrides previous description", t => {
  let struct = S.object(o =>
    o->S.field(
      "optionalField",
      S.string()
      ->JSONSchema.description("Previous description")
      ->S.deprecated(~message="Use another field", ()),
    )
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Ok(
      %raw(`{
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "additionalProperties": true,
      }`),
    ),
    (),
  )
})

test("Schema of object struct with nested object", t => {
  let struct = S.object(o =>
    o->S.field("objectWithOneStringField", S.object(o => o->S.field("Field", S.string())))
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
  let struct = S.object(o => (
    o->S.field("field", S.string()),
    o->S.field("optionalField", S.option(S.string())),
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
  let struct = S.option(S.string())

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported at root",
    ),
    (),
  )
})

test("Make JSONSchema throws error with object field wrapped in option multiple times", t => {
  let struct = S.object(o => o->S.field("optionalOptionalField", S.option(S.option(S.string()))))

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["optionalOptionalField"]. Reason: Optional struct is not supported inside the Option struct`),
    (),
  )
})

test("Primitive struct schema with description", t => {
  let struct = S.bool()->JSONSchema.description("A primitive struct")

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
  let struct = S.object(o =>
    o->S.field(
      "field",
      S.option(
        S.bool()->S.transform(
          ~parser=bool => {
            switch bool {
            | true => "true"
            | false => ""
            }
          },
          (),
        ),
      )->S.defaulted("true"),
    )
  )

  t->Assert.deepEqual(
    JSONSchema.make(struct),
    Error(`[ReScript JSON Schema] Failed converting at ["field"]. Reason: Couldn't destruct default value. Error: Failed serializing at root. Reason: Struct serializer is missing`),
    (),
  )
})

test("Transformed struct schema uses default with correct type", t => {
  let struct = S.object(o =>
    o->S.field(
      "field",
      S.option(
        S.bool()->S.transform(
          ~parser=bool => {
            switch bool {
            | true => "true"
            | false => ""
            }
          },
          ~serializer=string => {
            switch string {
            | "true" => true
            | _ => false
            }
          },
          (),
        ),
      )->S.defaulted("true"),
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
  let struct = S.bool()->JSONSchema.extend({description: "foo"})

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
    S.bool()
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
  let struct = S.object(o =>
    o->S.field(
      "optionalField",
      S.option(S.string())->JSONSchema.extend({"nullable": true}->Obj.magic),
    )
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
  let struct = S.unknown()

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
    JSONSchema.make(S.dict(S.option(S.string()))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Dict item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.array(S.option(S.string()))),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Array item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.union([S.option(S.string()), S.null(S.string())])),
    Error(
      "[ReScript JSON Schema] Failed converting at root. Reason: Optional struct is not supported as Union item",
    ),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.make(S.tuple1(. S.option(S.string()))),
    Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional struct is not supported as Tuple item`),
    (),
  )
})

module Example = {
  @live
  type author = {id: float, tags: array<string>, isAproved: bool, deprecatedAge: option<int>}

  test("Example", t => {
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

    t->Assert.deepEqual(
      JSONSchema.make(authorStruct),
      Ok(
        %raw(`{
          '$schema': 'http://json-schema.org/draft-07/schema#',
          additionalProperties: true,
          properties: {
            Age: {
              deprecated: true,
              description: 'Will be removed in APIv2',
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
