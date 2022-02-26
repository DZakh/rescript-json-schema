open Ava
open S

test("Guard works with literal schema", t => {
  let struct = bool
  let jsonSchema = JsonSchema.make(struct)

  let ajv = Ajv.make()
  let boolValidator = ajv->Ajv.Validator.make(jsonSchema)

  t->Assert.is(boolValidator->Ajv.Validator.is(true), true, ())
  t->Assert.is(boolValidator->Ajv.Validator.is(123), false, ())
})

module RecordSchemaGuard = {
  type user = {name: string, email: option<string>, age: int}

  let validateUser = data => {
    let struct = record3(
      ~fields=(field("Name", string), field("Email", option(string)), field("Age", int)),
      ~decode=((name, email, age)) => {name: name, email: email, age: age},
    )
    let jsonSchema = JsonSchema.make(struct)

    let ajv = Ajv.make()
    let userValidator = ajv->Ajv.Validator.make(jsonSchema)
    userValidator->Ajv.Validator.is(data)
  }

  test("[Record schema guard] Record with all valid fields is valid", t => {
    t->Assert.is(
      validateUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)),
      true,
      (),
    )
  })

  test(
    "[Record schema guard] Record with additional field that not described in schema is valid",
    t => {
      t->Assert.is(
        validateUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21,"Height":186}`)),
        true,
        (),
      )
    },
  )

  test("[Record schema guard] Record with valid fields and missing optional field is valid", t => {
    t->Assert.is(validateUser(%raw(`{"Name":"Dmitry","Age":21}`)), true, ())
  })

  test("[Record schema guard] Record with missing required field is invalid", t => {
    t->Assert.is(validateUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com"}`)), false, ())
  })

  test(
    "[Record schema guard] Record with different field names is invalid, even though it's the rescript record described by struct",
    t => {
      t->Assert.is(
        validateUser({name: "Dmitry", email: Some("dzakh.dev@gmail.com"), age: 21}),
        false,
        (),
      )
    },
  )
}
