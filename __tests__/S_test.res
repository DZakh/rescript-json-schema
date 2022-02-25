open Ava
open S

type recordWithOneStringField = {field1: string}
type recordWithOneOptionalStringField = {optionalField1: option<string>}

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
  let struct = record1(~fields=field("field1", string), ~decode=field1 => {field1: field1})

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})

test("Schema of record struct with one optional string field", t => {
  let struct = record1(~fields=field("optionalField1", option(string)), ~decode=optionalField1 => {
    optionalField1: optionalField1,
  })

  switch JsonSchema.make(struct) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->JsonSchema.valueOf, ())
  }
})
