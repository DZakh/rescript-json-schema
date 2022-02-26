type unknown = Js.Json.t
external unsafeToUnknown: 'unknown => unknown = "%identity"

// TODO: Add title and description
type rec struct<'value> = {kind: kind<'value>, decoder: option<unknown => 'value>}
and kind<_> =
  | String: kind<string>
  | Int: kind<int>
  | Float: kind<float>
  | Bool: kind<bool>
  | Option(struct<'value>): kind<option<'value>>
  | Array(struct<'value>): kind<array<'value>>
  // TODO: Add nullable
  // TODO: Add raw
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
and field<'value> = (string, struct<'value>)

let make = (~kind, ~decoder=?, ()): struct<'value> => {
  {kind: kind, decoder: decoder}
}

let classify = struct => struct.kind

external unsafeDecoder: unknown => 'value = "%identity"
let _decode:
  type value. (struct<value>, unknown) => value =
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
function(fields, construct, decode) {
  var isSingleField = typeof fields[0] === "string";
  if (isSingleField) {
    return function(unknown) {
      var fieldName = fields[0],
        fieldStruct = fields[1];
      return construct(decode(fieldStruct, unknown[fieldName]));
    }
  }
  return function(unknown) {
    var ctx = [];
    fields.forEach(function (field) {
      var fieldName = field[0],
        fieldStruct = field[1];
      ctx.push(decode(fieldStruct, unknown[fieldName]));
    })
    return construct(ctx);
  }
}
`)

  let make = (~fields: 'fields, ~construct: 'ctx => 'value, unknown: unknown): 'value => {
    _make(~fields, ~construct, ~decode=_decode)(unknown)
  }
}

let string = make(~kind=String, ())
let bool = make(~kind=Bool, ())
let int = make(~kind=Int, ())
let float = make(~kind=Float, ())

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

let record1 = (~fields, ~construct) => {
  make(~kind=Record1(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record2 = (~fields, ~construct) => {
  make(~kind=Record2(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record3 = (~fields, ~construct) => {
  make(~kind=Record3(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record4 = (~fields, ~construct) => {
  make(~kind=Record4(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record5 = (~fields, ~construct) => {
  make(~kind=Record5(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record6 = (~fields, ~construct) => {
  make(~kind=Record6(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record7 = (~fields, ~construct) => {
  make(~kind=Record7(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record8 = (~fields, ~construct) => {
  make(~kind=Record8(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
let record9 = (~fields, ~construct) => {
  make(~kind=Record9(fields), ~decoder=RecordDecoder.make(~fields, ~construct), ())
}
