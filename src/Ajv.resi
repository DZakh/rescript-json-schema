type ajv
let make: unit => ajv

module Validator: {
  type t<'value>

  let make: (ajv, S.t<'value>) => t<'value>

  let is: (t<'value>, Js.Json.t) => bool

  let parse: (t<'value>, Js.Json.t) => result<'value, string>
}
