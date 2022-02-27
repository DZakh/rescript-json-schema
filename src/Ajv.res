type ajv
@new @module external make: unit => ajv = "ajv"

module Validator = {
  type ajvValidator
  type t<'value> = {struct: S.t<'value>, ajvValidator: ajvValidator}

  @send external _make: (ajv, JsonSchema.t<'value>) => ajvValidator = "compile"
  let make = (ajv, struct) => {
    let jsonSchema = JsonSchema.make(struct)
    let ajvValidator = ajv->_make(jsonSchema)
    {ajvValidator: ajvValidator, struct: struct}
  }

  let _is = %raw(`
    function(validator, data) {
      return validator(data)
    }
  `)

  let is = (self: t<'value>, unknown: S.unknown): bool => {
    _is(self.ajvValidator, unknown)
  }

  // TODO: Add validate function with validation error messages

  let parse = (self: t<'value>, unknown: S.unknown): result<'value, unit> => {
    switch self->is(unknown) {
    | true => Ok(self.struct->S.decode(unknown))
    // TODO: Properly handle errors
    | false => Error()
    }
  }
}
