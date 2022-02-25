type struct<'value, 'ctx>
type field<'value, 'ctx>

let string: struct<string, string>
let bool: struct<bool, bool>
let int: struct<int, int>
let float: struct<float, float>

let field: (string, struct<'value, 'ctx>) => field<'value, 'ctx>

let option: struct<'value, 'ctx> => struct<option<'value>, option<'ctx>>

let record1: (~fields: field<'v1, 'c1>, ~decode: 'v1 => 'value) => struct<'value, 'v1>
let record2: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>),
  ~decode: (('v1, 'v2)) => 'value,
) => struct<'value, ('v1, 'v2)>
let record3: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>),
  ~decode: (('v1, 'v2, 'v3)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3)>
let record4: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>, field<'v4, 'c4>),
  ~decode: (('v1, 'v2, 'v3, 'v4)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4)>
let record5: (
  ~fields: (field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>, field<'v4, 'c4>, field<'v5, 'c5>),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5)>
let record6: (
  ~fields: (
    field<'v1, 'c1>,
    field<'v2, 'c2>,
    field<'v3, 'c3>,
    field<'v4, 'c4>,
    field<'v5, 'c5>,
    field<'v6, 'c6>,
  ),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6)>
let record7: (
  ~fields: (
    field<'v1, 'c1>,
    field<'v2, 'c2>,
    field<'v3, 'c3>,
    field<'v4, 'c4>,
    field<'v5, 'c5>,
    field<'v6, 'c6>,
    field<'v7, 'c7>,
  ),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7)>
let record8: (
  ~fields: (
    field<'v1, 'c1>,
    field<'v2, 'c2>,
    field<'v3, 'c3>,
    field<'v4, 'c4>,
    field<'v5, 'c5>,
    field<'v6, 'c6>,
    field<'v7, 'c7>,
    field<'v8, 'c8>,
  ),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8)>
let record9: (
  ~fields: (
    field<'v1, 'c1>,
    field<'v2, 'c2>,
    field<'v3, 'c3>,
    field<'v4, 'c4>,
    field<'v5, 'c5>,
    field<'v6, 'c6>,
    field<'v7, 'c7>,
    field<'v8, 'c8>,
    field<'v9, 'c9>,
  ),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9)>
let record10: (
  ~fields: (
    field<'v1, 'c1>,
    field<'v2, 'c2>,
    field<'v3, 'c3>,
    field<'v4, 'c4>,
    field<'v5, 'c5>,
    field<'v6, 'c6>,
    field<'v7, 'c7>,
    field<'v8, 'c8>,
    field<'v9, 'c9>,
    field<'v10, 'c10>,
  ),
  ~decode: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9, 'v10)) => 'value,
) => struct<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9, 'v10)>

module JsonSchema: {
  type error = [#UnknownJsonSchemaError | #RootOptionJsonSchemaError | #NestedOptionJsonSchemaError]
  type t<'value>
  type json<'value>

  let make: struct<'value, 'ctx> => result<t<'value>, error>

  let valueOf: t<'value> => json<'value>
}
