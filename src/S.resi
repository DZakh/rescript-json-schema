type unknown = Js.Json.t

type rec struct<'value>
and kind<_> =
  | String: kind<string>
  | Int: kind<int>
  | Float: kind<float>
  | Bool: kind<bool>
  | Option(struct<'value>): kind<option<'value>>
  | Array(struct<'value>): kind<array<'value>>
  | Record1(field<'v1>): kind<'value>
  | Record2((field<'v1>, field<'v2>)): kind<'value>
  | Record3((field<'v1>, field<'v2>, field<'v3>)): kind<'value>
  | Record4((field<'v1>, field<'v2>, field<'v3>, field<'v4>)): kind<'value>
  | Record5((field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>)): kind<'value>
  | Record6((field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>)): kind<'value>
  | Record7(
      (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>, field<'v7>),
    ): kind<'value>
  | Record8(
      (
        field<'v1>,
        field<'v2>,
        field<'v3>,
        field<'v4>,
        field<'v5>,
        field<'v6>,
        field<'v7>,
        field<'v8>,
      ),
    ): kind<'value>
  | Record9(
      (
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
    ): kind<'value>
and field<'value> = (string, struct<'value>)

let unsafeToUnknown: 'unknown => unknown

let classify: struct<'value> => kind<'value>

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
