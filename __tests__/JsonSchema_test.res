open Ava

external unsafeToJsonSchema: 'unknown => JsonSchema.t = "%identity"

type recordWithOneStringField = {field: string}
type recordWithOneOptionalStringField = {optionalField: option<string>}
type recordWithOneOptionalOptionalStringField = {optionalOptionalField: option<option<string>>}
type recordWithOneOptionalAndOneRequiredStringField = {optionalField: option<string>, field: string}
type nestedRecord = {recordWithOneStringField: recordWithOneStringField}

test("Schema of bool struct", t => {
  let struct = S.bool()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}->unsafeToJsonSchema,
    (),
  )
})

test("Schema of string struct", t => {
  let struct = S.string()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}->unsafeToJsonSchema,
    (),
  )
})

test("Schema of int struct", t => {
  let struct = S.int()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}->unsafeToJsonSchema,
    (),
  )
})

test("Schema of float struct", t => {
  let struct = S.float()

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}->unsafeToJsonSchema,
    (),
  )
})

test("Schema of strings array struct", t => {
  let struct = S.array(S.string())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "array",
      "items": {"type": "string"},
    }->unsafeToJsonSchema,
    (),
  )
})

test("Schema of strings dict struct", t => {
  let struct = S.dict(S.string())

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "additionalProperties": {"type": "string"},
    }->unsafeToJsonSchema,
    (),
  )
})

test("Schema of record struct with one string field", t => {
  let struct = S.record1(
    ~fields=("field", S.string()),
    ~constructor=field => {field: field}->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {"field": {"type": "string"}},
      "required": ["field"],
    }->unsafeToJsonSchema,
    (),
  )
})

test("Schema of record struct with one optional string field", t => {
  let struct = S.record1(
    ~fields=("optionalField", S.option(S.string())),
    ~constructor=optionalField =>
      {
        optionalField: optionalField,
      }->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {"optionalField": {"type": "string"}},
    }->unsafeToJsonSchema,
    (),
  )
})

test("Schema of record struct with nested record", t => {
  let struct = S.record1(
    ~fields=(
      "recordWithOneStringField",
      S.record1(~fields=("Field", S.string()), ~constructor=field => {field: field}->Ok, ()),
    ),
    ~constructor=recordWithOneStringField =>
      {
        recordWithOneStringField: recordWithOneStringField,
      }->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {
        "recordWithOneStringField": {
          "type": "object",
          "properties": {"Field": {"type": "string"}},
          "required": ["Field"],
        },
      },
      "required": ["recordWithOneStringField"],
    }->unsafeToJsonSchema,
    (),
  )
})

test("Schema of record struct with one optional and one required string field", t => {
  let struct = S.record2(
    ~fields=(("field", S.string()), ("optionalField", S.option(S.string()))),
    ~constructor=((field, optionalField)) =>
      {
        field: field,
        optionalField: optionalField,
      }->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
        "optionalField": {"type": "string"},
      },
      "required": ["field"],
    }->unsafeToJsonSchema,
    (),
  )
})

test("Make JsonSchema throws error with optional root type", t => {
  let struct = S.option(S.string())

  t->Assert.throws(() => {
    JsonSchema.make(struct)->ignore
  }, ~expectations=ThrowsException.make(~message="The root struct can\'t be optional", ()), ())
})

test("Make JsonSchema throws error with record field wrapped in option multiple times", t => {
  let struct = S.record1(
    ~fields=("optionalOptionalField", S.option(S.option(S.string()))),
    ~constructor=optionalOptionalField =>
      {
        optionalOptionalField: optionalOptionalField,
      }->Ok,
    (),
  )

  t->Assert.throws(() => {
    JsonSchema.make(struct)->ignore
  }, ~expectations=ThrowsException.make(
    ~message="The option struct can\'t be nested in another option struct",
    (),
  ), ())
})

test("Primitive struct schema with description", t => {
  let struct = S.bool()->JsonSchema.description("A primitive struct")

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "boolean",
      "description": "A primitive struct",
    }->unsafeToJsonSchema,
    (),
  )
})

test("Primitive struct schema with additional raw schema", t => {
  let struct = S.bool()->JsonSchema.raw(JsonSchema.Raw.make({"nullable": true}))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "boolean",
      "nullable": true,
    }->unsafeToJsonSchema,
    (),
  )
})

test("Multiple additional raw schemas are merged together", t => {
  let struct =
    S.bool()
    ->JsonSchema.raw(JsonSchema.Raw.make({"nullable": true}))
    ->JsonSchema.raw(JsonSchema.Raw.make({"deprecated": true}))

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "boolean",
      "deprecated": true,
      "nullable": true,
    }->unsafeToJsonSchema,
    (),
  )
})

test("Additional raw schema works with optional fields", t => {
  let struct = S.record1(
    ~fields=(
      "optionalField",
      S.option(S.string())->JsonSchema.raw(JsonSchema.Raw.make({"nullable": true})),
    ),
    ~constructor=optionalField =>
      {
        optionalField: optionalField,
      }->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {
        "optionalField": {"nullable": true, "type": "string"},
      },
    }->unsafeToJsonSchema,
    (),
  )
})

test("Custom struct doesn't affect final schema", t => {
  let struct = S.custom(
    ~constructor=unknown => unknown->Js.Json.decodeString->Belt.Option.getWithDefault("")->Ok,
    (),
  )

  t->Assert.deepEqual(
    JsonSchema.make(struct),
    {
      "$schema": "http://json-schema.org/draft-07/schema#",
    }->unsafeToJsonSchema,
    (),
  )
})
