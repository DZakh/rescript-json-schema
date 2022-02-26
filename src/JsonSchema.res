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
  type value. S.struct<value> => meta<value> =
  struct => {
    switch struct->S.classify {
    | S.String => Required(FJS.string())
    | S.Int => Required(FJS.integer())
    | S.Bool => Required(FJS.boolean())
    | S.Float => Required(FJS.number())
    | S.Array(s') =>
      Required(FJS.array()->FJS.items(makeMetaSchema(s')->applyMetaData(~isRecordField=false)))
    | S.Option(s') =>
      switch makeMetaSchema(s') {
      | Optional(_) => raise(NestedOptionException)
      | Required(s'') => Optional(s'')
      }
    | S.Record1((fn1, fs1)) =>
      Required(FJS.object()->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData))
    | S.Record2((fn1, fs1), (fn2, fs2)) =>
      Required(
        FJS.object()
        ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
        ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData),
      )
    | S.Record3((fn1, fs1), (fn2, fs2), (fn3, fs3)) =>
      Required(
        FJS.object()
        ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
        ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
        ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData),
      )
    | S.Record4((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4)) =>
      Required(
        FJS.object()
        ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
        ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
        ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
        ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData),
      )
    | S.Record5((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4), (fn5, fs5)) =>
      Required(
        FJS.object()
        ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
        ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
        ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
        ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
        ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData),
      )
    | S.Record6((fn1, fs1), (fn2, fs2), (fn3, fs3), (fn4, fs4), (fn5, fs5), (fn6, fs6)) =>
      Required(
        FJS.object()
        ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
        ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
        ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData)
        ->FJS.prop(fn4, makeMetaSchema(fs4)->applyMetaData)
        ->FJS.prop(fn5, makeMetaSchema(fs5)->applyMetaData)
        ->FJS.prop(fn6, makeMetaSchema(fs6)->applyMetaData),
      )
    | S.Record7(
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
    | S.Record8(
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
    | S.Record9(
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
