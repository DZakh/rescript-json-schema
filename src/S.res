type unknown = Js.Json.t
external unsafeToUnknown: 'unknown => unknown = "%identity"

// TODO: Add title and description (probably not here)
type rec t<'value> = {
  kind: kind<'value>,
  decoder: option<unknown => 'value>,
  meta: Js.Dict.t<unknown>,
}
and kind<_> =
  | String: kind<string>
  | Int: kind<int>
  | Float: kind<float>
  | Bool: kind<bool>
  | Option(t<'value>): kind<option<'value>>
  | Array(t<'value>): kind<array<'value>>
  // TODO: Add nullable
  // TODO: Add custom
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

let make = (~kind, ~decoder=?, ()): t<'value> => {
  {kind: kind, decoder: decoder, meta: Js.Dict.empty()}
}

@module
external mergeMeta: (unknown, unknown) => unknown = "deepmerge"

let classify = struct => struct.kind
let getMeta = (struct, ~namespace) => {
  let maybeExistingMeta = struct.meta->Js.Dict.get(namespace)
  switch maybeExistingMeta {
  | Some(existingMeta) => existingMeta
  | None => Js.Dict.empty()->Js.Json.object_
  }
}
let mixinMeta = (struct, ~namespace, ~meta) => {
  let maybeExistingMeta = struct.meta->Js.Dict.get(namespace)
  let nextMeta = switch maybeExistingMeta {
  | Some(existingMeta) => mergeMeta(existingMeta, meta)
  | None => meta
  }
  struct.meta->Js.Dict.set(namespace, nextMeta)
  struct
}

external unsafeDecoder: unknown => 'value = "%identity"
let _decode:
  type value. (t<value>, unknown) => value =
  (struct, unknown) => {
    switch struct.decoder {
    | Some(decoder) => unknown->decoder
    | None => unknown->unsafeDecoder
    }
  }

let decode = _decode
let decodeWith = (unknown, struct) => {
  _decode(struct, unknown)
}

module RecordDecoder = {
  let _make = %raw(`
function(fields, constructor, decode) {
  var isSingleField = typeof fields[0] === "string";
  if (isSingleField) {
    return function(unknown) {
      var fieldName = fields[0],
        fieldStruct = fields[1];
      return constructor(decode(fieldStruct, unknown[fieldName]));
    }
  }
  return function(unknown) {
    var ctx = [];
    fields.forEach(function (field) {
      var fieldName = field[0],
        fieldStruct = field[1];
      ctx.push(decode(fieldStruct, unknown[fieldName]));
    })
    return constructor(ctx);
  }
}
`)

  let make = (~fields: 'fields, ~constructor: 'ctx => 'value, unknown: unknown): 'value => {
    _make(~fields, ~constructor, ~decode=_decode)(unknown)
  }
}

let string = () => make(~kind=String, ())
let bool = () => make(~kind=Bool, ())
let int = () => make(~kind=Int, ())
let float = () => make(~kind=Float, ())

external unsafeUnknownToArray: unknown => array<unknown> = "%identity"
let array = struct =>
  make(
    ~kind=Array(struct),
    ~decoder=unknown => {
      unknown->unsafeUnknownToArray->Js.Array2.map(_decode(struct))
    },
    (),
  )

external unsafeUnknownToOption: unknown => option<unknown> = "%identity"
let option = struct => {
  make(
    ~kind=Option(struct),
    ~decoder=unknown => {
      switch unknown->unsafeUnknownToOption {
      | Some(unknown') => Some(_decode(struct, unknown'))
      | None => None
      }
    },
    (),
  )
}

let record1 = (~fields, ~constructor) => {
  make(~kind=Record1(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record2 = (~fields, ~constructor) => {
  make(~kind=Record2(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record3 = (~fields, ~constructor) => {
  make(~kind=Record3(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record4 = (~fields, ~constructor) => {
  make(~kind=Record4(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record5 = (~fields, ~constructor) => {
  make(~kind=Record5(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record6 = (~fields, ~constructor) => {
  make(~kind=Record6(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record7 = (~fields, ~constructor) => {
  make(~kind=Record7(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record8 = (~fields, ~constructor) => {
  make(~kind=Record8(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
let record9 = (~fields, ~constructor) => {
  make(~kind=Record9(fields), ~decoder=RecordDecoder.make(~fields, ~constructor), ())
}
