module FJS = {
  type t<'v>
  type json<'v>
  @module("fluent-json-schema")
  external object: unit => t<option<'v>> = "object"
  @module("fluent-json-schema")
  external string: unit => t<option<string>> = "string"
  @module("fluent-json-schema")
  external int: unit => t<option<int>> = "integer"
  @module("fluent-json-schema")
  external bool: unit => t<option<bool>> = "boolean"

  @send external prop: (t<'v>, string, t<'p>) => t<'v> = "prop"
  @send external required: (t<option<'v>>, unit) => t<'v> = "required"
  @send external valueOf: t<'v> => json<'v> = "valueOf"
}

type rec struct<'value, 'ctx> = {typ: typ<'value, 'ctx>, decode: 'ctx => 'value}
and typ<_, _> =
  | String: typ<string, string>
  | Int: typ<int, int>
  | Bool: typ<bool, bool>
  | Option(struct<'value, 'ctx>): typ<option<'value>, option<'ctx>>
  | Object1(field<'v1, 'c1>): typ<'value, 'v1>
  | Object2(field<'v1, 'c1>, field<'v2, 'c2>): typ<'value, ('v1, 'v2)>
  | Object3(field<'v1, 'c1>, field<'v2, 'c2>, field<'v3, 'c3>): typ<'value, ('v1, 'v2, 'v3)>
and field<'value, 'ctx> = (string, struct<'value, 'ctx>)

let make = (~typ, ~decode, ()): struct<'value, 'ctx> => {
  {typ: typ, decode: decode}
}

let string = make(~typ=String, ~decode=v => v, ())
let bool = make(~typ=Bool, ~decode=v => v, ())
let int = make(~typ=Int, ~decode=v => v, ())

let field = (fieldName, fieldSchema) => {
  (fieldName, fieldSchema)
}

let option = s => {
  make(~typ=Option(s), ~decode=ctx => ctx->Belt.Option.mapU((. ctx') => s.decode(ctx')), ())
}

let object1 = (~fields, ~decode) => {
  let f1 = fields
  make(~typ=Object1(f1), ~decode, ())
}
let object2 = (~fields, ~decode) => {
  let (f1, f2) = fields
  make(~typ=Object2(f1, f2), ~decode, ())
}
let object3 = (~fields, ~decode) => {
  let (f1, f2, f3) = fields
  make(~typ=Object3(f1, f2, f3), ~decode, ())
}

// TODO: Properly handle error
exception Exception(string)

module JsonSchema = {
  type json<'value> = FJS.json<'value>
  type rec t<_> = JsonSchema({fluentSchema: FJS.t<'value>, struct: struct<'value, 'ctx>}): t<'value>
  type rec fluentSchema<'value> = FJS.t<'value>
  and meta<'value> =
    | Optional(fluentSchema<'value>)
    | Required(fluentSchema<option<'value>>)
    | RootObject(fluentSchema<option<'value>>)
  external unwrapRootObjectValueType: fluentSchema<option<'value>> => fluentSchema<'value> =
    "%identity"

  let applyMetaData = (state: meta<'value>): fluentSchema<'value> => {
    switch state {
    | Optional(fluentSchema) => fluentSchema
    | RootObject(fluentSchema) => fluentSchema->unwrapRootObjectValueType
    | Required(fluentSchema) => fluentSchema->FJS.required()
    }
  }

  let rec makeMetaSchema:
    type src ctx. struct<src, ctx> => meta<src> =
    s => {
      switch s.typ {
      | String => Required(FJS.string())
      | Int => Required(FJS.int())
      | Bool => Required(FJS.bool())
      | Option(s') =>
        switch makeMetaSchema(s') {
        | Optional(_) => raise(Exception("TODO:"))
        | Required(s'') => Optional(s'')
        | RootObject(s'') => Optional(s'')
        }
      | Object1((fn1, fs1)) =>
        RootObject(FJS.object()->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData))
      | Object2((fn1, fs1), (fn2, fs2)) =>
        RootObject(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData),
        )
      | Object3((fn1, fs1), (fn2, fs2), (fn3, fs3)) =>
        RootObject(
          FJS.object()
          ->FJS.prop(fn1, makeMetaSchema(fs1)->applyMetaData)
          ->FJS.prop(fn2, makeMetaSchema(fs2)->applyMetaData)
          ->FJS.prop(fn3, makeMetaSchema(fs3)->applyMetaData),
        )
      }
    }

  let make = struct => {
    let fluentSchema = makeMetaSchema(struct)->applyMetaData
    JsonSchema({
      fluentSchema: fluentSchema,
      struct: struct,
    })
  }

  let valueOf = jsonSchema => {
    let JsonSchema(jsonSchema') = jsonSchema
    jsonSchema'.fluentSchema->FJS.valueOf
  }
}
