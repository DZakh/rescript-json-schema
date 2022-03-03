%%raw(`class RestructError extends Error {}`)
let raiseRestructError = %raw(`function(message){
  throw new RestructError(message);
}`)

module ResultX = {
  let mapError = (result, fn) =>
    switch result {
    | Ok(_) as ok => ok
    | Error(error) => Error(fn(error))
    }

  let sequence = (results: array<result<'ok, 'error>>): result<array<'ok>, 'error> => {
    results->Js.Array2.reduce((maybeAcc, res) => {
      maybeAcc->Belt.Result.flatMap(acc => res->Belt.Result.map(x => acc->Js.Array2.concat([x])))
    }, Ok([]))
  }
}

module Error = {
  type locationComponent = Field(string) | Index(int)

  type location = array<locationComponent>

  type rec t = {kind: kind, mutable location: location}
  and kind =
    | MissingDecoder
    // | MissingEncoder
    | DecodingFailed(string)

  module MissingDecoder = {
    let make = () => {
      {kind: MissingDecoder, location: []}
    }
  }

  module DecodingFailed = {
    let make = reason => {
      {kind: DecodingFailed(reason), location: []}
    }
  }

  let formatLocation = location =>
    "." ++
    location
    ->Js.Array2.map(s =>
      switch s {
      | Field(field) => `"` ++ field ++ `"`
      | Index(index) => `[` ++ index->Js.Int.toString ++ `]`
      }
    )
    ->Js.Array2.joinWith(".")

  let prependLocation = (error, location) => {
    error.location = [location]->Js.Array2.concat(error.location)
    error
  }

  let toString = error => {
    let withLocationInfo = error.location->Js.Array2.length !== 0
    switch (error.kind, withLocationInfo) {
    | (MissingDecoder, true) => `Struct missing decoder at ${error.location->formatLocation}`
    | (MissingDecoder, false) => `Struct missing decoder at root`
    // | (MissingEncoder, true) => `Struct missing encoder at ${error.location->formatLocation}`
    // | (MissingEncoder, false) => `Struct missing encoder at root`
    | (DecodingFailed(reason), true) =>
      `Struct decoding failed at ${error.location->formatLocation}. Reason: ${reason}`
    | (DecodingFailed(reason), false) => `Struct decoding failed at root. Reason: ${reason}`
    }
  }
}

type unknown = Js.Json.t
external unsafeToUnknown: 'unknown => unknown = "%identity"

// TODO: Add title and description (probably not here)
type rec t<'value> = {
  kind: kind<'value>,
  decoder: option<unknown => result<'value, Error.t>>,
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
  type value. (t<value>, unknown) => result<value, Error.t> =
  (struct, unknown) => {
    switch struct.decoder {
    | Some(decoder) => unknown->decoder
    | None => unknown->unsafeDecoder->Ok
    }
  }

let decode = (struct, unknown) => {
  _decode(struct, unknown)->ResultX.mapError(Error.toString)
}
let decodeWith = (unknown, struct) => {
  decode(struct, unknown)
}

module Record = {
  type t<'value, 'fields, 'fieldValues> = {decoder: unknown => result<'value, Error.t>}

  exception HackyAbort(Error.t)

  let _decoder = %raw(`
function(fields, constructor, decode) {
  var isSingleField = typeof fields[0] === "string";
  if (isSingleField) {
    return function(unknown) {
      var fieldName = fields[0],
        fieldStruct = fields[1];
      return constructor(decode(fieldStruct, fieldName, unknown[fieldName]));
    }
  }
  return function(unknown) {
    var ctx = [];
    fields.forEach(function (field) {
      var fieldName = field[0],
        fieldStruct = field[1];
      ctx.push(decode(fieldStruct, fieldName, unknown[fieldName]));
    })
    return constructor(ctx);
  }
}
`)

  let make = (
    ~fields: 'fields,
    ~constructor: option<'fieldValues => result<'value, string>>,
    ~destructor: option<'value => result<'fieldValues, string>>,
  ): t<'value, 'fields, 'fieldValues> => {
    if constructor->Belt.Option.isNone && destructor->Belt.Option.isNone {
      raiseRestructError("For a Record struct either a constructor, or a destructor is required")
    }

    {
      decoder: unknown => {
        switch constructor {
        | Some(constructor) =>
          try {
            _decoder(~fields, ~constructor, ~decode=(struct, fieldName, fieldValue) => {
              switch _decode(struct, fieldValue) {
              | Ok(value) => value
              | Error(error) =>
                raise(HackyAbort(error->Error.prependLocation(Error.Field(fieldName))))
              }
            })(unknown)->ResultX.mapError(Error.DecodingFailed.make)
          } catch {
          | HackyAbort(error) => Error(error)
          }
        | None => Error.MissingDecoder.make()->Error
        }
      },
    }
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
      unknown
      ->unsafeUnknownToArray
      ->Js.Array2.mapi((unknownItem, idx) => {
        struct->_decode(unknownItem)->ResultX.mapError(Error.prependLocation(_, Error.Index(idx)))
      })
      ->ResultX.sequence
    },
    (),
  )

external unsafeUnknownToOption: unknown => option<unknown> = "%identity"
let option = struct => {
  make(
    ~kind=Option(struct),
    ~decoder=unknown => {
      switch unknown->unsafeUnknownToOption {
      | Some(unknown') => _decode(struct, unknown')->Belt.Result.map(known => Some(known))
      | None => Ok(None)
      }
    },
    (),
  )
}

let record1 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record1(fields), ~decoder, ())
}
let record2 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record2(fields), ~decoder, ())
}
let record3 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record3(fields), ~decoder, ())
}
let record4 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record4(fields), ~decoder, ())
}
let record5 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record5(fields), ~decoder, ())
}
let record6 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record6(fields), ~decoder, ())
}
let record7 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record7(fields), ~decoder, ())
}
let record8 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record8(fields), ~decoder, ())
}
let record9 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {decoder} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record9(fields), ~decoder, ())
}
