type struct<'value, 'ctx>
type field<'value, 'ctx>

let string: struct<string, string>
let bool: struct<bool, bool>
let int: struct<int, int>
let float: struct<float, float>

let field: (string, struct<'value, 'ctx>) => field<'value, 'ctx>

let array: struct<'value, 'value> => struct<array<'value>, array<'value>>
let option: struct<'value, 'value> => struct<option<'value>, option<'value>>

let record1: (~fields: field<'v1, 'c1>, ~construct: 'v1 => 'value) => struct<'value, 'v1>
let record2: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>),
  ~construct: (('v1, 'v2)) => 'value,
) => struct<'value, ('v1, 'v2)>
let record3: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>),
  ~construct: (('v1, 'v2, 'v3)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3)>

let decode: (struct<'value, 'ctx>, Js.Json.t) => 'value
let decodeWith: (Js.Json.t, struct<'value, 'ctx>) => 'value

module JsonSchema: {
  type t<'value>

  let make: struct<'value, 'ctx> => t<'value>
}
