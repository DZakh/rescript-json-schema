open Ava

type recordWithOneStringField = {field: string}
type recordWithOneOptionalStringField = {optionalField: option<string>}
type recordWithOneOptionalOptionalStringField = {optionalOptionalField: option<option<string>>}
type recordWithOneOptionalAndOneRequiredStringField = {optionalField: option<string>, field: string}
type throwsExpectation = {message: option<string>}

test("Schema of bool struct", t => {
  let struct = S.bool

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of string struct", t => {
  let struct = S.string

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of int struct", t => {
  let struct = S.int

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of float struct", t => {
  let struct = S.float

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of strings array struct", t => {
  let struct = S.array(S.string)

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of record struct with one string field", t => {
  let struct = S.record1(~fields=("field", S.string), ~construct=field => {field: field})

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of record struct with one optional string field", t => {
  let struct = S.record1(
    ~fields=("optionalField", S.option(S.string)),
    ~construct=optionalField => {
      optionalField: optionalField,
    },
  )

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

test("Schema of record struct with one optional and one required string field", t => {
  let struct = S.record2(
    ~fields=(("field", S.string), ("optionalField", S.option(S.string))),
    ~construct=((field, optionalField)) => {
      field: field,
      optionalField: optionalField,
    },
  )

  t->Assert.snapshot(S.JsonSchema.make(struct), ())
})

Ava.test("Make JsonSchema throws error with optional root type", t => {
  let struct = S.option(S.string)

  t->Assert.throws(
    () => {
      S.JsonSchema.make(struct)->ignore
    },
    ~expectations={
      message: Some("The root struct can\'t be optional."),
    },
    (),
  )
})

Ava.test("Make JsonSchema throws error with record field wrapped in option multiple times", t => {
  let struct = S.record1(
    ~fields=("optionalOptionalField", S.option(S.option(S.string))),
    ~construct=optionalOptionalField => {
      optionalOptionalField: optionalOptionalField,
    },
  )

  t->Assert.throws(
    () => {
      S.JsonSchema.make(struct)->ignore
    },
    ~expectations={
      message: Some("The option struct can\'t be nested in another option struct."),
    },
    (),
  )
})
