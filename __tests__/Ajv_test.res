open Ava

test("Guard works with primitive schema", t => {
  let struct = S.bool

  let ajv = Ajv.make()
  let boolValidator = ajv->Ajv.Validator.make(struct)

  t->Assert.is(boolValidator->Ajv.Validator.is(true->S.unsafeToUnknown), true, ())
  t->Assert.is(boolValidator->Ajv.Validator.is(123->S.unsafeToUnknown), false, ())
})

failing("Guard works with array of optional primitives schema", t => {
  let struct = S.array(S.option(S.bool))

  let ajv = Ajv.make()
  let boolValidator = ajv->Ajv.Validator.make(struct)

  t->Assert.is(
    boolValidator->Ajv.Validator.is(%raw(`[true, undefined]`)->S.unsafeToUnknown),
    true,
    (),
  )
})

module TestRecordSchemaGuard = {
  type user = {name: string, email: option<string>, age: int}

  let validateUser = data => {
    let struct = S.record3(
      ~fields=(("Name", S.string), ("Email", S.option(S.string)), ("Age", S.int)),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age},
    )

    let ajv = Ajv.make()
    let userValidator = ajv->Ajv.Validator.make(struct)
    userValidator->Ajv.Validator.is(data->S.unsafeToUnknown)
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

module TestRecordSchemaParse = {
  type user = {name: string, email: option<string>, age: int}

  let parseUser = data => {
    let struct = S.record3(
      ~fields=(("Name", S.string), ("Email", S.option(S.string)), ("Age", S.int)),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age},
    )

    let ajv = Ajv.make()
    let userValidator = ajv->Ajv.Validator.make(struct)
    userValidator->Ajv.Validator.parse(data->S.unsafeToUnknown)
  }

  test("[Record schema parse] Record with all valid fields", t => {
    t->Assert.deepEqual(
      parseUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)),
      Ok({name: "Dmitry", email: Some("dzakh.dev@gmail.com"), age: 21}),
      (),
    )
  })

  test("[Record schema parse] Record with additional field that not described in schema", t => {
    t->Assert.deepEqual(
      parseUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21,"Height":186}`)),
      Ok({name: "Dmitry", email: Some("dzakh.dev@gmail.com"), age: 21}),
      (),
    )
  })

  test("[Record schema parse] Record with valid fields and missing optional field", t => {
    t->Assert.deepEqual(
      parseUser(%raw(`{"Name":"Dmitry","Age":21}`)),
      Ok({name: "Dmitry", email: None, age: 21}),
      (),
    )
  })

  test("[Record schema parse] Record with missing required field is invalid", t => {
    t->Assert.deepEqual(
      parseUser(%raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com"}`)),
      Error(),
      (),
    )
  })
}
