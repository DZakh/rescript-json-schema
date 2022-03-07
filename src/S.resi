type unknown = Js.Json.t

type rec t<'value>
and kind<_> =
  | String: kind<string>
  | Int: kind<int>
  | Float: kind<float>
  | Bool: kind<bool>
  | Option(t<'value>): kind<option<'value>>
  | Array(t<'value>): kind<array<'value>>
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
and field<'value> = (string, t<'value>)

let string: unit => t<string>
let bool: unit => t<bool>
let int: unit => t<int>
let float: unit => t<float>
let array: t<'value> => t<array<'value>>
let option: t<'value> => t<option<'value>>

let record1: (
  ~fields: field<'v1>,
  ~constructor: 'v1 => result<'value, string>=?,
  ~destructor: 'value => result<'v1, string>=?,
  unit,
) => t<'value>
let record2: (
  ~fields: (field<'v1>, field<'v2>),
  ~constructor: (('v1, 'v2)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2), string>=?,
  unit,
) => t<'value>
let record3: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>),
  ~constructor: (('v1, 'v2, 'v3)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3), string>=?,
  unit,
) => t<'value>
let record4: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>),
  ~constructor: (('v1, 'v2, 'v3, 'v4)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4), string>=?,
  unit,
) => t<'value>
let record5: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>),
  ~constructor: (('v1, 'v2, 'v3, 'v4, 'v5)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4, 'v5), string>=?,
  unit,
) => t<'value>
let record6: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>),
  ~constructor: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4, 'v5, 'v6), string>=?,
  unit,
) => t<'value>
let record7: (
  ~fields: (field<'v1>, field<'v2>, field<'v3>, field<'v4>, field<'v5>, field<'v6>, field<'v7>),
  ~constructor: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7), string>=?,
  unit,
) => t<'value>
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
  ~constructor: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8), string>=?,
  unit,
) => t<'value>
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
  ~constructor: (('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9)) => result<'value, string>=?,
  ~destructor: 'value => result<('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9), string>=?,
  unit,
) => t<'value>

let construct: (t<'value>, unknown) => result<'value, string>
let constructWith: (unknown, t<'value>) => result<'value, string>

let unsafeToUnknown: 'any => unknown
let classify: t<'value> => kind<'value>
let getMeta: (t<'value>, ~namespace: string) => unknown
let mixinMeta: (t<'value>, ~namespace: string, ~meta: unknown) => t<'value>
