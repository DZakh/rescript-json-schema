module FJS = {
  type t<'v>
  type json<'v>
  @module("fluent-json-schema")
  external object: unit => t<option<'v>> = "object"
  @module("fluent-json-schema")
  external string: unit => t<option<string>> = "string"
  @module("fluent-json-schema")
  external integer: unit => t<option<int>> = "integer"
  @module("fluent-json-schema")
  external boolean: unit => t<option<bool>> = "boolean"
  @module("fluent-json-schema")
  external number: unit => t<option<float>> = "number"
  @module("fluent-json-schema")
  external array: unit => t<option<array<'item>>> = "array"

  @send external prop: (t<'v>, string, t<'p>) => t<'v> = "prop"
  @send external required: (t<option<'v>>, unit) => t<'v> = "required"
  @send external items: (t<option<array<'item>>>, t<'item>) => t<option<array<'item>>> = "items"
  @send external valueOf: t<'v> => json<'v> = "valueOf"
}

type rec struct<'value, 'ctx> = {typ: typ<'value, 'ctx>, decode: 'ctx => 'value}
and typ<_, _> =
  | String: typ<string, string>
  | Int: typ<int, int>
  | Float: typ<float, float>
  | Bool: typ<bool, bool>
  | Option(struct<'value, 'ctx>): typ<option<'value>, option<'ctx>>
  | Array(struct<'value, 'ctx>): typ<array<'value>, array<'ctx>>
  | Record1(field<'v1, 'c1>): typ<'value, 'v1>
  | Record2(field<'v1, 'c1>, field<'v2, 'c2>): typ<'value, ('v1, 'v2)>
  | Record3(field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>): typ<'value, ('v1, 'v2, 'v3)>
  | Record4(field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>, field<'v4, 'c4>): typ<
      'value,
      ('v1, 'v2, 'v3, 'v4),
    >
  | Record5(
      field<'v1, 'c1>,
      field<'v2, 'c2>,
      field<'v3, 'c3>,
      field<'v4, 'c4>,
      field<'v5, 'c5>,
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5)>
  | Record6(
      field<'v1, 'c1>,
      field<'v2, 'c2>,
      field<'v3, 'c3>,
      field<'v4, 'c4>,
      field<'v5, 'c5>,
      field<'v6, 'c6>,
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6)>
  | Record7(
      field<'v1, 'c1>,
      field<'v2, 'c2>,
      field<'v3, 'c3>,
      field<'v4, 'c4>,
      field<'v5, 'c5>,
      field<'v6, 'c6>,
      field<'v7, 'c7>,
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7)>
  | Record8(
      field<'v1, 'c1>,
      field<'v2, 'c2>,
      field<'v3, 'c3>,
      field<'v4, 'c4>,
      field<'v5, 'c5>,
      field<'v6, 'c6>,
      field<'v7, 'c7>,
      field<'v8, 'c8>,
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8)>
  | Record9(
      field<'v1, 'c1>,
      field<'v2, 'c2>,
      field<'v3, 'c3>,
      field<'v4, 'c4>,
      field<'v5, 'c5>,
      field<'v6, 'c6>,
      field<'v7, 'c7>,
      field<'v8, 'c8>,
      field<'v9, 'c9>,
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9)>
  | Record10(
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
    ): typ<'value, ('v1, 'v2, 'v3, 'v4, 'v5, 'v6, 'v7, 'v8, 'v9, 'v10)>
and field<'value, 'ctx> = (string, struct<'value, 'ctx>)

let make = (~typ, ~decode, ()): struct<'value, 'ctx> => {
  {typ: typ, decode: decode}
}

let string = make(~typ=String, ~decode=v => v, ())
let bool = make(~typ=Bool, ~decode=v => v, ())
let int = make(~typ=Int, ~decode=v => v, ())
let float = make(~typ=Float, ~decode=v => v, ())

let field = (fieldName, fieldSchema) => {
  (fieldName, fieldSchema)
}

let array = struct => make(~typ=Array(struct), ~decode=v => v, ())
let option = struct => {
  make(~typ=Option(struct), ~decode=v => v, ())
}

let record1 = (~fields, ~decode) => {
  let f1 = fields
  make(~typ=Record1(f1), ~decode, ())
}
let record2 = (~fields, ~decode) => {
  let (f1, f2) = fields
  make(~typ=Record2(f1, f2), ~decode, ())
}
let record3 = (~fields, ~decode) => {
  let (f1, f2, f3) = fields
  make(~typ=Record3(f1, f2, f3), ~decode, ())
}
let record4 = (~fields, ~decode) => {
  let (f1, f2, f3, f4) = fields
  make(~typ=Record4(f1, f2, f3, f4), ~decode, ())
}
let record5 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5) = fields
  make(~typ=Record5(f1, f2, f3, f4, f5), ~decode, ())
}
let record6 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5, f6) = fields
  make(~typ=Record6(f1, f2, f3, f4, f5, f6), ~decode, ())
}
let record7 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5, f6, f7) = fields
  make(~typ=Record7(f1, f2, f3, f4, f5, f6, f7), ~decode, ())
}
let record8 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5, f6, f7, f8) = fields
  make(~typ=Record8(f1, f2, f3, f4, f5, f6, f7, f8), ~decode, ())
}
let record9 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5, f6, f7, f8, f9) = fields
  make(~typ=Record9(f1, f2, f3, f4, f5, f6, f7, f8, f9), ~decode, ())
}
let record10 = (~fields, ~decode) => {
  let (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = fields
  make(~typ=Record10(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10), ~decode, ())
}

module JsonSchema = {
  exception NestedOptionException
  exception RootOptionException

  type t<'value> = FJS.json<'value>
  type rec fluentSchema<'value> = FJS.t<'value>
  and meta<'value> =
    | Optional(fluentSchema<'value>)
    | Required(fluentSchema<option<'value>>)

  external unwrapRootValueType: fluentSchema<option<'value>> => fluentSchema<'value> = "%identity"

  let applyMetaData = (~isRecordField=true, meta: meta<'value>): fluentSchema<'value> => {
    switch (meta, isRecordField) {
    | (Optional(_), false) => raise(RootOptionException)
    | (Optional(fluentSchema), true) => fluentSchema
    | (Required(fluentSchema), false) => fluentSchema->unwrapRootValueType
    | (Required(fluentSchema), true) => fluentSchema->FJS.required()
    }
  }

  let rec makeMetaSchema:
    type src ctx. struct<src, ctx> => meta<src> =
    s => {
      switch s.typ {
      | String => Required(FJS.string())
      | Int => Required(FJS.integer())
      | Bool => Required(FJS.boolean())
      | Float => Required(FJS.number())
      | Array(s') =>
        Required(FJS.array()->FJS.items(makeMetaSchema(s')->applyMetaData(~isRecordField=false)))
      | Option(s') =>
        switch makeMetaSchema(s') {
        | Optional(_) => raise(NestedOptionException)
        | Required(s'') => Optional(s'')
        }
      | Record1((fn1, fs1)) =>
        Required(FJS.object()->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData))
      | Record2((fn1, fs1), (fn2, fs2)) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData),
        )
      | Record3((fn1, fs1), (fn2, fs2), (fn3, fs3)) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData),
        )
      | Record4((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4)) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData),
        )
      | Record5((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4), (fn5, fs5)) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData),
        )
      | Record6((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4), (fn5, fs5), (fn6, fs6)) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
          ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData),
        )
      | Record7(
          (fn1, fs1),
          (fn2, fs2),
          (fn3, fs3),
          (fn4, fs4),
          (fn5, fs5),
          (fn6, fs6),
          (fn7, fs7),
        ) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
          ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData)
          ->FJS.prop(fn7, makeMetaSchema(fs7)->applyMetaData),
        )
      | Record8(
          (fn1, fs1),
          (fn2, fs2),
          (fn3, fs3),
          (fn4, fs4),
          (fn5, fs5),
          (fn6, fs6),
          (fn7, fs7),
          (fn8, fs8),
        ) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
          ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData)
          ->FJS.prop(fn7, makeMetaSchema(fs7)->applyMetaData)
          ->FJS.prop(fn8, makeMetaSchema(fs8)->applyMetaData),
        )
      | Record9(
          (fn1, fs1),
          (fn2, fs2),
          (fn3, fs3),
          (fn4, fs4),
          (fn5, fs5),
          (fn6, fs6),
          (fn7, fs7),
          (fn8, fs8),
          (fn9, fs9),
        ) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
          ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData)
          ->FJS.prop(fn7, makeMetaSchema(fs7)->applyMetaData)
          ->FJS.prop(fn8, makeMetaSchema(fs8)->applyMetaData)
          ->FJS.prop(fn9, makeMetaSchema(fs9)->applyMetaData),
        )
      | Record10(
          (fn1, fs1),
          (fn2, fs2),
          (fn3, fs3),
          (fn4, fs4),
          (fn5, fs5),
          (fn6, fs6),
          (fn7, fs7),
          (fn8, fs8),
          (fn9, fs9),
          (fn10, fs10),
        ) =>
        Required(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
          ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
          ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
          ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData)
          ->FJS.prop(fn7, makeMetaSchema(fs7)->applyMetaData)
          ->FJS.prop(fn8, makeMetaSchema(fs8)->applyMetaData)
          ->FJS.prop(fn9, makeMetaSchema(fs9)->applyMetaData)
          ->FJS.prop(fn10, makeMetaSchema(fs10)->applyMetaData),
        )
      }
    }

  let make = struct => {
    try {
      let fluentSchema = makeMetaSchema(struct)->applyMetaData(~isRecordField=false)
      fluentSchema->FJS.valueOf
    } catch {
    | NestedOptionException =>
      Js.Exn.raiseError("The option struct can't be nested in another option struct.")
    | RootOptionException => Js.Exn.raiseError("The root struct can't be optional.")
    // TODO: Handle FluentSchema error
    // TODO: Raise custom instance of error
    | _ => Js.Exn.raiseError("Unknown RescriptJsonSchema error.")
    }
  }
}
