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
    | MissingConstructor
    // | MissingEncoder
    | ConstructingFailed(string)

  module MissingConstructor = {
    let make = () => {
      {kind: MissingConstructor, location: []}
    }
  }

  module ConstructingFailed = {
    let make = reason => {
      {kind: ConstructingFailed(reason), location: []}
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
    | (MissingConstructor, true) =>
      `Struct missing constructor at ${error.location->formatLocation}`
    | (MissingConstructor, false) => `Struct missing constructor at root`
    // | (MissingEncoder, true) => `Struct missing encoder at ${error.location->formatLocation}`
    // | (MissingEncoder, false) => `Struct missing encoder at root`
    | (ConstructingFailed(reason), true) =>
      `Struct constructing failed at ${error.location->formatLocation}. Reason: ${reason}`
    | (ConstructingFailed(reason), false) => `Struct constructing failed at root. Reason: ${reason}`
    }
  }
}

type unknown = Js.Json.t
external unsafeToUnknown: 'unknown => unknown = "%identity"

// TODO: Add title and description (probably not here)
type rec t<'value> = {
  kind: kind<'value>,
  constructor: option<unknown => result<'value, Error.t>>,
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

let make = (~kind, ~constructor=?, ()): t<'value> => {
  {kind: kind, constructor: constructor, meta: Js.Dict.empty()}
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

let _construct = (struct, unknown) => {
  switch struct.constructor {
  | Some(constructor) => unknown->constructor
  | None => Error.MissingConstructor.make()->Error
  }
}

let construct = (struct, unknown) => {
  _construct(struct, unknown)->ResultX.mapError(Error.toString)
}
let constructWith = (unknown, struct) => {
  construct(struct, unknown)
}

module Record = {
  type t<'value, 'fields, 'fieldValues> = {constructor: option<unknown => result<'value, Error.t>>}

  exception HackyAbort(Error.t)

  let _constructor = %raw(`
function(fields, constructor, construct) {
  var isSingleField = typeof fields[0] === "string";
  if (isSingleField) {
    return function(unknown) {
      var fieldName = fields[0],
        fieldStruct = fields[1];
      return constructor(construct(fieldStruct, fieldName, unknown[fieldName]));
    }
  }
  return function(unknown) {
    var ctx = [];
    fields.forEach(function (field) {
      var fieldName = field[0],
        fieldStruct = field[1];
      ctx.push(construct(fieldStruct, fieldName, unknown[fieldName]));
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
      constructor: constructor->Belt.Option.map(constructor => {
        unknown => {
          try {
            _constructor(~fields, ~constructor, ~construct=(struct, fieldName, fieldValue) => {
              switch _construct(struct, fieldValue) {
              | Ok(value) => value
              | Error(error) =>
                raise(HackyAbort(error->Error.prependLocation(Error.Field(fieldName))))
              }
            })(unknown)->ResultX.mapError(Error.ConstructingFailed.make)
          } catch {
          | HackyAbort(error) => Error(error)
          }
        }
      }),
    }
  }
}

external unsafeConstructor: unknown => 'value = "%identity"
let defaultPrimitiveConstructor = (unknown: unknown) => {
  unknown->unsafeConstructor->Ok
}

let string = () => make(~kind=String, ~constructor=defaultPrimitiveConstructor, ())
let bool = () => make(~kind=Bool, ~constructor=defaultPrimitiveConstructor, ())
let int = () => make(~kind=Int, ~constructor=defaultPrimitiveConstructor, ())
let float = () => make(~kind=Float, ~constructor=defaultPrimitiveConstructor, ())

external unsafeUnknownToArray: unknown => array<unknown> = "%identity"
let array = struct =>
  make(
    ~kind=Array(struct),
    ~constructor=unknown => {
      unknown
      ->unsafeUnknownToArray
      ->Js.Array2.mapi((unknownItem, idx) => {
        struct
        ->_construct(unknownItem)
        ->ResultX.mapError(Error.prependLocation(_, Error.Index(idx)))
      })
      ->ResultX.sequence
    },
    (),
  )

external unsafeUnknownToOption: unknown => option<unknown> = "%identity"
let option = struct => {
  make(
    ~kind=Option(struct),
    ~constructor=unknown => {
      switch unknown->unsafeUnknownToOption {
      | Some(unknown') => _construct(struct, unknown')->Belt.Result.map(known => Some(known))
      | None => Ok(None)
      }
    },
    (),
  )
}

let record1 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record1(fields), ~constructor?, ())
}
let record2 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record2(fields), ~constructor?, ())
}
let record3 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record3(fields), ~constructor?, ())
}
let record4 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record4(fields), ~constructor?, ())
}
let record5 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record5(fields), ~constructor?, ())
}
let record6 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record6(fields), ~constructor?, ())
}
let record7 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record7(fields), ~constructor?, ())
}
let record8 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record8(fields), ~constructor?, ())
}
let record9 = (~fields, ~constructor=?, ~destructor=?, ()) => {
  let {constructor} = Record.make(~fields, ~constructor, ~destructor)
  make(~kind=Record9(fields), ~constructor?, ())
}
