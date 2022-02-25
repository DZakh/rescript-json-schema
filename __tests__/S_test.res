open Ava

test("Schema of bool struct", t => {
  switch S.JsonSchema.make(S.bool) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->S.JsonSchema.valueOf, ())
  }
})

test("Schema of string struct", t => {
  switch S.JsonSchema.make(S.string) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->S.JsonSchema.valueOf, ())
  }
})

test("Schema of int struct", t => {
  switch S.JsonSchema.make(S.int) {
  | Error(error) => t->Assert.fail(`Shouldn't be error. Error: ${(error :> string)}`)
  | Ok(jsonSchema) => t->Assert.snapshot(jsonSchema->S.JsonSchema.valueOf, ())
  }
})
