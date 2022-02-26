open Ava
open S

test("Validation works with literal schema", t => {
  let struct = bool
  let jsonSchema = JsonSchema.make(struct)

  let ajv = Ajv.make()
  let boolValidator = ajv->Ajv.Validator.make(jsonSchema)

  t->Assert.is(boolValidator->Ajv.Validator.validate(true), true, ())
  t->Assert.is(boolValidator->Ajv.Validator.validate(123), false, ())
})
