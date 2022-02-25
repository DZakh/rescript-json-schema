type struct<'value, 'ctx>
type field<'value, 'ctx>

let string: struct<string, string>
let bool: struct<bool, bool>
let int: struct<int, int>

let field: (string, struct<'value, 'ctx>) => field<'value, 'ctx>

let option: struct<'value, 'ctx> => struct<option<'value>, option<'ctx>>

let object1: (~fields: field<'v1, 'c1>, ~decode: 'v1 => 'value) => struct<'value, 'v1>
let object2: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>),
  ~decode: (('v1, 'v2)) => 'value,
) => struct<'value, ('v1, 'v2)>
let object3: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>),
  ~decode: (('v1, 'v2, 'v3)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3)>

module JsonSchema: {
  type error = [#JSON_SCHEMA_UNKNOWN_ERROR]
  type t<'value>
  type json<'value>

  let make: struct<'value, 'ctx> => result<t<'value>, error>

  let valueOf: t<'value> => json<'value>
}
