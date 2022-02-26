type ajv
@new @module external make: unit => ajv = "ajv"

module Validator = {
  type t<'value>

  @send external make: (ajv, S.JsonSchema.t<'value>) => t<'value> = "compile"

  let validate = %raw(`
    function(validator, data) {
      return validator(data)
    }
  `)
}
