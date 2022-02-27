type ajv
let make: unit => ajv

module Validator: {
  type t<'value>

  let make: (ajv, S.t<'value>) => t<'value>

  let is: (t<'value>, S.unknown) => bool

  let parse: (t<'value>, S.unknown) => result<'value, unit>
}
