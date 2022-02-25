open Ava
open S

type recordWithOneStringField = {field: string}
type recordWithOneOptionalStringField = {optionalField: option<string>}
type recordWithOneOptionalAndOneRequiredStringField = {optionalField: option<string>, field: string}

test("Schema of bool struct", t => {
  let struct = bool

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of string struct", t => {
  let struct = string

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of int struct", t => {
  let struct = int

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of float struct", t => {
  let struct = float

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of record struct with one string field", t => {
  let struct = record1(~fields=field("field", string), ~decode=field => {field: field})

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of record struct with one optional string field", t => {
  let struct = record1(~fields=field("optionalField", option(string)), ~decode=optionalField => {
    optionalField: optionalField,
  })

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of record struct with one optional and one required string field", t => {
  let struct = record2(
    ~fields=(field("field", string), field("optionalField", option(string))),
    ~decode=((field, optionalField)) => {
      field: field,
      optionalField: optionalField,
    },
  )

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})
