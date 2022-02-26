type unknown = Js.Json.t
type struct<'value>
type field<'value> = (string, struct<'value>)

let unsafeToUnknown: 'unknown => unknown

let string: struct<string>
let bool: struct<bool>
let int: struct<int>
let float: struct<float>

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
let record4: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>),
  ~construct: (('v1, 'v2, 'v3, 'v4)) => 'value,
) => struct<'value>
let record5: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>),
  ~construct: (('v1, 'v2, 'v3, 'v4, 'v5)) => 'value,
) => struct<'value>
let record6: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>),
  ~construct: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6)) => 'value,
) => struct<'value>
let record7: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>, field<'v7>),
  ~construct: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7)) => 'value,
) => struct<'value>
let record8: (
  ~fields: (
    field<'v1>,
    field<'v2>,
    field<'v3>,
    field<'v4>,
    field<'v5>,
    field<'v6>,
    field<'v7>,
    field<'v8>,
  ),
  ~construct: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8)) => 'value,
) => struct<'value>
let record9: (
  ~fields: (
    field<'v1>,
    field<'v2>,
    field<'v3>,
    field<'v4>,
    field<'v5>,
    field<'v6>,
    field<'v7>,
    field<'v8>,
    field<'v9>,
  ),
  ~construct: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9)) => 'value,
) => struct<'value>

let decode: (struct<'value>, unknown) => 'value
let decodeWith: (unknown, struct<'value>) => 'value

module JsonSchema: {
  type t<'value>

  let make: struct<'value> => t<'value>
}
