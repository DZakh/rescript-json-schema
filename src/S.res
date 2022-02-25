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

  @send external prop: (t<'v>, string, t<'p>) => t<'v> = "prop"
  @send external required: (t<option<'v>>, unit) => t<'v> = "required"
  @send external valueOf: t<'v> => json<'v> = "valueOf"
}

type rec struct<'value, 'ctx> = {typ: typ<'value, 'ctx>, decode: 'ctx => 'value}
and typ<_, _> =
  | String: typ<string, string>
  | Int: typ<int, int>
  | Float: typ<float, float>
  | Bool: typ<bool, bool>
  | Option(struct<'value, 'ctx>): typ<option<'value>, option<'ctx>>
  | Record1(field<'v1, 'c1>): typ<'value, 'v1>
  | Record2(field<'v1, 'c1>, field<'v2, 'c2>): typ<'value, ('v1, 'v2)>
  | Record3(field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>): typ<'value, ('v1, 'v2, 'v3)>
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

let option = s => {
  make(~typ=Option(s), ~decode=ctx => ctx->Belt.Option.mapU((. ctx') => s.decode(ctx')), ())
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

module JsonSchema = {
  exception NestedOptionException
  exception RootOptionException

  type error = [#UnknownJsonSchemaError | #RootOptionJsonSchemaError | #NestedOptionJsonSchemaError]
  type json<'value> = FJS.json<'value>
  type rec t<_> =
    | JsonSchema({
        fluentSchema: FJS.t<'value>,
        struct: struct<'value, 'ctx>,
        json: json<'value>,
      }): t<'value>
  type rec fluentSchema<'value> = FJS.t<'value>
  and meta<'value> =
    | Optional(fluentSchema<'value>)
    | Required(fluentSchema<option<'value>>)

  external unwrapRootValueType: fluentSchema<option<'value>> => fluentSchema<'value> = "%identity"

  let applyMetaData = (~isRoot=false, meta: meta<'value>): fluentSchema<'value> => {
    switch (meta, isRoot) {
    | (Optional(_), true) => raise(RootOptionException)
    | (Optional(fluentSchema), false) => fluentSchema
    | (Required(fluentSchema), true) => fluentSchema->unwrapRootValueType
    | (Required(fluentSchema), false) => fluentSchema->FJS.required()
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
      }
    }

  let make = struct => {
    try {
      let fluentSchema = makeMetaSchema(struct)->applyMetaData(~isRoot=true)
      let json = fluentSchema->FJS.valueOf
      Ok(
        JsonSchema({
          fluentSchema: fluentSchema,
          struct: struct,
          json: json,
        }),
      )
    } catch {
    | NestedOptionException => Error(#NestedOptionJsonSchemaError)
    | RootOptionException => Error(#RootOptionJsonSchemaError)
    | _ => Error(#UnknownJsonSchemaError)
    }
  }

  let valueOf = jsonSchema => {
    let JsonSchema(jsonSchema') = jsonSchema
    jsonSchema'.json
  }
}
