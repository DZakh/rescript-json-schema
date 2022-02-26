type ajv
@new @module external make: unit => ajv = "ajv"

module Validator = {
  type t<'value>

  @send external make: (ajv, S.JsonSchema.t<'value>) => t<'value> = "compile"

  let is = %raw(`
    function(validator, data) {
      return validator(data)
    }
  `)
}
