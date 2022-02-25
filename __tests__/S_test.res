open Ava
open S

type recordWithOneStringField = {field: string}
type recordWithOneOptionalStringField = {optionalField: option<string>}
type recordWithOneOptionalOptionalStringField = {optionalOptionalField: option<option<string>>}
type recordWithOneOptionalAndOneRequiredStringField = {optionalField: option<string>, field: string}
type throwsExpectation = {message: option<string>}

test("Schema of bool struct", t => {
  let struct = bool

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of string struct", t => {
  let struct = string

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of int struct", t => {
  let struct = int

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of float struct", t => {
  let struct = float

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of record struct with one string field", t => {
  let struct = record1(~fields=field("field", string), ~decode=field => {field: field})

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of record struct with one optional string field", t => {
  let struct = record1(~fields=field("optionalField", option(string)), ~decode=optionalField => {
    optionalField: optionalField,
  })

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

test("Schema of record struct with one optional and one required string field", t => {
  let struct = record2(
    ~fields=(field("field", string), field("optionalField", option(string))),
    ~decode=((field, optionalField)) => {
      field: field,
      optionalField: optionalField,
    },
  )

  t->Assert.snapshot(JsonSchema.make(struct)->JsonSchema.valueOf, ())
})

Ava.test("Make JsonSchema throws error with optional root type", t => {
  let struct = option(string)

  t->Assert.throws(
    () => {
      JsonSchema.make(struct)->JsonSchema.valueOf->ignore
    },
    ~expectations={
      message: Some("The root struct can\'t be optional."),
    },
    (),
  )
})

Ava.test("Make JsonSchema throws error with record field wrapped in option multiple times", t => {
  let struct = record1(
    ~fields=field("optionalOptionalField", option(option(string))),
    ~decode=optionalOptionalField => {
      optionalOptionalField: optionalOptionalField,
    },
  )

  t->Assert.throws(
    () => {
      JsonSchema.make(struct)->JsonSchema.valueOf->ignore
    },
    ~expectations={
      message: Some("The option struct can\'t be nested in another option struct."),
    },
    (),
  )
})
