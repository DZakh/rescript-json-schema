type ajv
let make: unit => ajv

module Validator: {
  type t<'value>

  let make: (ajv, S.JsonSchema.t<'value>) => t<'value>

  let validate: (t<'value>, 'data) => bool
}
