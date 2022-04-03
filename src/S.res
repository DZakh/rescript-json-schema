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
      Js.log(reason)
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

  let _constructor = %raw(`function(fields, customConstructor, construct) {
    var isSingleField = typeof fields[0] === "string";
    if (isSingleField) {
      return function(unknown) {
        var fieldName = fields[0],
          fieldStruct = fields[1],
          fieldValue = construct(fieldStruct, fieldName, unknown[fieldName]);
        return customConstructor(fieldValue);
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
      return customConstructor(fieldValues);
    }
  }`)

  let _destructor = %raw(`function(fields, tupleDestructor, destruct) {
    var isSingleField = typeof fields[0] === "string";
    if (isSingleField) {
      return function(value) {
        var fieldName = fields[0],
          fieldStruct = fields[1],
          fieldValue = tupleDestructor(value),
          unknownFieldValue = destruct(fieldStruct, fieldName, fieldValue);
        return {
          [fieldName]: unknownFieldValue,
        };
      }
    }
    return function(value) {
      var unknown = {},
        fieldValuesTuple = tupleDestructor(value);
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

  let make = (
    ~fields: 'fields,
    ~customConstructor: option<'fieldValues => result<'value, string>>,
    ~customDestructor: option<'value => result<'fieldValues, string>>,
  ): t<'value> => {
    if customConstructor->Belt.Option.isNone && customDestructor->Belt.Option.isNone {
      raiseRestructError("For a Record struct either a constructor, or a destructor is required")
    }

    make(
      ~kind=Record(_mapTupleToUnsafeArray(fields)),
      ~constructor=?customConstructor->Belt.Option.map(customConstructor => {
        unknown => {
          try {
            _constructor(~fields, ~customConstructor, ~construct=(
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
      ~destructor=?customDestructor->Belt.Option.map(customDestructor => {
        value => {
          try {
            _destructor(
              ~fields,
              ~customDestructor=value => {
                switch customDestructor(value) {
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
  let make = (~customConstructor, ~customDestructor, kind) => {
    if customConstructor->Belt.Option.isNone && customDestructor->Belt.Option.isNone {
      raiseRestructError("For a Coerced struct either a constructor, or a destructor is required")
    }

    make(
      ~kind,
      ~constructor=?customConstructor->Belt.Option.map(customConstructor => {
        unknown => {
          customConstructor(unknown->unsafeFromUnknown)->ResultX.mapError(
            Error.ConstructingFailed.make,
          )
        }
      }),
      ~destructor=?customDestructor->Belt.Option.map(customDestructor => {
        value => {
          switch customDestructor(value) {
          | Ok(primitive) => primitive->unsafeToUnknown->Ok
          | Error(reason) => Error.DestructingFailed.make(reason)->Error
          }
        }
      }),
      (),
    )
  }
}

module Primitive = {
  let make = kind => {
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

let string = () => Primitive.make(String)
let coercedString = (
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => {
  CoercedPrimitive.make(~customConstructor, ~customDestructor, String)
}
let bool = () => Primitive.make(Bool)
let coercedBool = (~constructor as customConstructor=?, ~destructor as customDestructor=?, ()) => {
  CoercedPrimitive.make(~customConstructor, ~customDestructor, Bool)
}
let int = () => Primitive.make(Int)
let coercedInt = (~constructor as customConstructor=?, ~destructor as customDestructor=?, ()) => {
  CoercedPrimitive.make(~customConstructor, ~customDestructor, Int)
}
let float = () => Primitive.make(Float)
let coercedFloat = (~constructor as customConstructor=?, ~destructor as customDestructor=?, ()) => {
  CoercedPrimitive.make(~customConstructor, ~customDestructor, Float)
}

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

let record1 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record2 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record3 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record4 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record5 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record6 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record7 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record8 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record9 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)
let record10 = (
  ~fields,
  ~constructor as customConstructor=?,
  ~destructor as customDestructor=?,
  (),
) => Record.make(~fields, ~customConstructor, ~customDestructor)

module Lib = {
  let classify = struct => struct.kind

  let getMetadata = (struct, namespace) => {
    struct.metadata->Js.Dict.get(namespace)->Belt.Option.map(unsafeFromUnknown)
  }

  let setMetadata = (struct, namespace, metadata) => {
    struct.metadata->Js.Dict.set(namespace, metadata->unsafeToUnknown)
    struct
  }
}
