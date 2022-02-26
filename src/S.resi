type unknown = Js.Json.t
type struct<'value>
type field<'value>

let unsafeToUnknown: 'unknown => unknown

let string: struct<string>
let bool: struct<bool>
let int: struct<int>
let float: struct<float>

let field: (string, struct<'value>) => field<'value>

let array: struct<'value> => struct<array<'value>>
let option: struct<'value> => struct<option<'value>>

let record1: (~fields: field<'v1>, ~construct: 'v1 => 'value) => struct<'value>
let record2: (
  ~fields: (field<'v1>, field<'v2>),
  ~construct: (('v1, 'v2)) => 'value,
) => struct<'value>
let record3: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>),
  ~construct: (('v1, 'v2, 'v3)) => 'value,
) => struct<'value>

let decode: (struct<'value>, unknown) => 'value
let decodeWith: (unknown, struct<'value>) => 'value

module JsonSchema: {
  type t<'value>

  let make: struct<'value> => t<'value>
}
