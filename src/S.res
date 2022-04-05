%%raw(`class RestructError extends Error {}`)
let raiseRestructError = %raw(`function(message){
  throw new RestructError(message);
}`)

let _mapTupleToUnsafeArray = %raw(`function(tuple){
  var isSingleField = typeof tuple[0] === "string";
  return isSingleField ? [tuple] : tuple;
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
    | MissingDestructor
    | ConstructingFailed(string)
    | DestructingFailed(string)

  module MissingConstructor = {
    let make = () => {
      {kind: MissingConstructor, location: []}
    }
  }

  module MissingDestructor = {
    let make = () => {
      {kind: MissingDestructor, location: []}
    }
  }

  module ConstructingFailed = {
    let make = reason => {
      {kind: ConstructingFailed(reason), location: []}
    }
  }

  module DestructingFailed = {
    let make = reason => {
      {kind: DestructingFailed(reason), location: []}
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
    | (MissingDestructor, true) => `Struct missing destructor at ${error.location->formatLocation}`
    | (MissingDestructor, false) => `Struct missing destructor at root`
    | (ConstructingFailed(reason), true) =>
      `Struct construction failed at ${error.location->formatLocation}. Reason: ${reason}`
    | (ConstructingFailed(reason), false) => `Struct construction failed at root. Reason: ${reason}`
    | (DestructingFailed(reason), true) =>
      `Struct destruction failed at ${error.location->formatLocation}. Reason: ${reason}`
    | (DestructingFailed(reason), false) => `Struct destruction failed at root. Reason: ${reason}`
    }
  }
}

type unknown = Js.Json.t

external unsafeToUnknown: 'unknown => unknown = "%identity"
external unsafeFromUnknown: unknown => 'value = "%identity"
external unsafeUnknownToArray: unknown => array<unknown> = "%identity"
external unsafeArrayToUnknown: array<unknown> => unknown = "%identity"
external unsafeUnknownToOption: unknown => option<unknown> = "%identity"
external unsafeOptionToUnknown: option<unknown> => unknown = "%identity"

// TODO: Add title and description (probably not here)
type constructor<'value> = unknown => result<'value, Error.t>
type destructor<'value> = 'value => result<unknown, Error.t>
type rec t<'value> = {
  kind: kind,
  constructor: option<constructor<'value>>,
  destructor: option<destructor<'value>>,
  metadata: Js.Dict.t<unknown>,
}
and kind =
  | String: kind
  | Int: kind
  | Float: kind
  | Bool: kind
  | Option(t<'value>): kind
  | Array(t<'value>): kind
  // TODO: Add nullable
  // TODO: Add custom
  | Record('unsafeFieldsArray): kind
and field<'value> = (string, t<'value>)

let make = (~kind, ~constructor=?, ~destructor=?, ()): t<'value> => {
  {kind: kind, constructor: constructor, destructor: destructor, metadata: Js.Dict.empty()}
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

let _destruct = (struct, unknown) => {
  switch struct.destructor {
  | Some(destructor) => unknown->destructor
  | None => Error.MissingDestructor.make()->Error
  }
}
let destruct = (struct, unknown) => {
  _destruct(struct, unknown)->ResultX.mapError(Error.toString)
}
let destructWith = (unknown, struct) => {
  destruct(struct, unknown)
}

module Record = {
  exception HackyAbort(Error.t)

  let _constructor = %raw(`function(fields, recordConstructor, construct) {
    var isSingleField = typeof fields[0] === "string";
    if (isSingleField) {
      return function(unknown) {
        var fieldName = fields[0],
          fieldStruct = fields[1],
          fieldValue = construct(fieldStruct, fieldName, unknown[fieldName]);
        return recordConstructor(fieldValue);
      }
    }
    return function(unknown) {
      var fieldValues = [];
      fields.forEach(function (field) {
        var fieldName = field[0],
          fieldStruct = field[1],
          fieldValue = construct(fieldStruct, fieldName, unknown[fieldName]);
        fieldValues.push(fieldValue);
      })
      return recordConstructor(fieldValues);
    }
  }`)

  let _destructor = %raw(`function(fields, recordDestructor, destruct) {
    var isSingleField = typeof fields[0] === "string";
    if (isSingleField) {
      return function(value) {
        var fieldName = fields[0],
          fieldStruct = fields[1],
          fieldValue = recordDestructor(value),
          unknownFieldValue = destruct(fieldStruct, fieldName, fieldValue);
        return {
          [fieldName]: unknownFieldValue,
        };
      }
    }
    return function(value) {
      var unknown = {},
        fieldValuesTuple = recordDestructor(value);
      fields.forEach(function (field, idx) {
        var fieldName = field[0],
          fieldStruct = field[1],
          fieldValue = fieldValuesTuple[idx],
          unknownFieldValue = destruct(fieldStruct, fieldName, fieldValue);
        unknown[fieldName] = unknownFieldValue;
      })
      return unknown;
    }
  }`)

  let factory = (
    ~fields: 'fields,
    ~constructor as maybeRecordConstructor: option<'fieldValues => result<'value, string>>=?,
    ~destructor as maybeRecordDestructor: option<'value => result<'fieldValues, string>>=?,
    (),
  ): t<'value> => {
    if maybeRecordConstructor->Belt.Option.isNone && maybeRecordDestructor->Belt.Option.isNone {
      raiseRestructError("For a Record struct either a constructor, or a destructor is required")
    }

    make(
      ~kind=Record(_mapTupleToUnsafeArray(fields)),
      ~constructor=?maybeRecordConstructor->Belt.Option.map(recordConstructor => {
        unknown => {
          try {
            _constructor(~fields, ~recordConstructor, ~construct=(
              struct,
              fieldName,
              unknownFieldValue,
            ) => {
              switch _construct(struct, unknownFieldValue) {
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
      ~destructor=?maybeRecordDestructor->Belt.Option.map(recordDestructor => {
        value => {
          try {
            _destructor(
              ~fields,
              ~recordDestructor=value => {
                switch recordDestructor(value) {
                | Ok(fieldValuesTuple) => fieldValuesTuple
                | Error(reason) => raise(HackyAbort(Error.DestructingFailed.make(reason)))
                }
              },
              ~destruct=(struct, fieldName, fieldValue) => {
                switch _destruct(struct, fieldValue) {
                | Ok(unknown) => unknown
                | Error(error) =>
                  raise(HackyAbort(error->Error.prependLocation(Error.Field(fieldName))))
                }
              },
            )(value)->Ok
          } catch {
          | HackyAbort(error) => Error(error)
          }
        }
      }),
      (),
    )
  }
}

module CoercedPrimitive = {
  module Factory = {
    let make = (
      ~kind: kind,
      ~constructor as maybePrimitiveConstructor: option<'primitive => result<'value, string>>=?,
      ~destructor as maybePrimitiveDestructor: option<'value => result<'primitive, string>>=?,
      (),
    ) => {
      if (
        maybePrimitiveConstructor->Belt.Option.isNone &&
          maybePrimitiveDestructor->Belt.Option.isNone
      ) {
        raiseRestructError("For a Coerced struct either a constructor, or a destructor is required")
      }

      make(
        ~kind,
        ~constructor=?maybePrimitiveConstructor->Belt.Option.map(primitiveConstructor => {
          unknown => {
            primitiveConstructor(unknown->unsafeFromUnknown)->ResultX.mapError(
              Error.ConstructingFailed.make,
            )
          }
        }),
        ~destructor=?maybePrimitiveDestructor->Belt.Option.map(primitiveDestructor => {
          value => {
            switch primitiveDestructor(value) {
            | Ok(primitive) => primitive->unsafeToUnknown->Ok
            | Error(reason) => Error.DestructingFailed.make(reason)->Error
            }
          }
        }),
        (),
      )
    }
  }
}

module Primitive = {
  module Factory = {
    let make = (~kind) => {
      () =>
        make(
          ~kind,
          ~constructor=unknown => {
            unknown->unsafeFromUnknown->Ok
          },
          ~destructor=value => {
            value->unsafeToUnknown->Ok
          },
          (),
        )
    }
  }
}

let string = Primitive.Factory.make(~kind=String)
let coercedString = (~constructor=?, ~destructor=?, ()) =>
  CoercedPrimitive.Factory.make(~kind=String, ~constructor?, ~destructor?, ())

let bool = Primitive.Factory.make(~kind=Bool)
let coercedBool = (~constructor=?, ~destructor=?, ()) =>
  CoercedPrimitive.Factory.make(~kind=Bool, ~constructor?, ~destructor?, ())

let int = Primitive.Factory.make(~kind=Int)
let coercedInt = (~constructor=?, ~destructor=?, ()) =>
  CoercedPrimitive.Factory.make(~kind=Int, ~constructor?, ~destructor?, ())

let float = Primitive.Factory.make(~kind=Float)
let coercedFloat = (~constructor=?, ~destructor=?, ()) =>
  CoercedPrimitive.Factory.make(~kind=Float, ~constructor?, ~destructor?, ())

// TODO: Reduce the number of interation for construction and destruction operations
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
    ~destructor=array => {
      array
      ->Js.Array2.mapi((item, idx) => {
        struct->_destruct(item)->ResultX.mapError(Error.prependLocation(_, Error.Index(idx)))
      })
      ->ResultX.sequence
      ->Belt.Result.map(unsafeArrayToUnknown)
    },
    (),
  )

let option = struct => {
  make(
    ~kind=Option(struct),
    ~constructor=unknown => {
      switch unknown->unsafeUnknownToOption {
      | Some(unknown) => _construct(struct, unknown)->Belt.Result.map(known => Some(known))
      | None => Ok(None)
      }
    },
    ~destructor=optionalValue => {
      switch optionalValue {
      | Some(value) => _destruct(struct, value)
      | None => Ok(None->unsafeOptionToUnknown)
      }
    },
    (),
  )
}

let record1 = Record.factory
let record2 = Record.factory
let record3 = Record.factory
let record4 = Record.factory
let record5 = Record.factory
let record6 = Record.factory
let record7 = Record.factory
let record8 = Record.factory
let record9 = Record.factory
let record10 = Record.factory

let classify = struct => struct.kind

let getMetadata = (struct, namespace) => {
  struct.metadata->Js.Dict.get(namespace)->Belt.Option.map(unsafeFromUnknown)
}

let setMetadata = (struct, namespace, metadata) => {
  struct.metadata->Js.Dict.set(namespace, metadata->unsafeToUnknown)
  struct
}
