module Stdlib = {
  module Result: {
    let mapError: (result<'ok, 'error1>, 'error1 => 'error2) => result<'ok, 'error2>
    let map: (result<'ok1, 'error>, 'ok1 => 'ok2) => result<'ok2, 'error>
    let flatMap: (result<'ok1, 'error>, 'ok1 => result<'ok2, 'error>) => result<'ok2, 'error>
    module Array: {
      let mapi: (array<'a>, (. 'a, int) => result<'b, 'e>) => result<array<'b>, 'e>
    }
  } = {
    @inline
    let mapError = (result, fn) =>
      switch result {
      | Ok(_) as ok => ok
      | Error(error) => Error(fn(error))
      }

    @inline
    let map = (result, fn) =>
      switch result {
      | Ok(value) => Ok(fn(value))
      | Error(_) as error => error
      }

    @inline
    let flatMap = (result, fn) =>
      switch result {
      | Ok(value) => fn(value)
      | Error(_) as error => error
      }

    module Array = {
      let mapi = (array, fn) => {
        let newArray = []
        let idxRef = ref(0)
        let maybeErrorRef = ref(None)

        while idxRef.contents < array->Js.Array2.length && maybeErrorRef.contents === None {
          let idx = idxRef.contents
          let item = array->Js.Array2.unsafe_get(idx)
          switch fn(. item, idx) {
          | Ok(value) => {
              newArray->Js.Array2.push(value)->ignore
              idxRef.contents = idxRef.contents + 1
            }

          | Error(_) as error => maybeErrorRef.contents = Some(error)
          }
        }

        switch maybeErrorRef.contents {
        | Some(error) => error
        | None => Ok(newArray)
        }
      }
    }
  }
}

module Error = {
  type rec t = {kind: kind, mutable path: array<string>}
  and kind =
    | UnsupportedNestedOptional
    | UnsupportedRootOptional
    | UnsupportedOptionalItem(string)
    | UnsupportedStruct(string)
    | DefaultDestructingFailed({destructingErrorMessage: string})

  module UnsupportedOptionalItem = {
    let make = struct => {
      {kind: UnsupportedOptionalItem(struct->S.name), path: []}
    }
  }

  module UnsupportedStruct = {
    let make = struct => {
      {kind: UnsupportedStruct(struct->S.name), path: []}
    }
  }

  module UnsupportedNestedOptional = {
    let make = () => {
      {kind: UnsupportedNestedOptional, path: []}
    }
  }

  module UnsupportedRootOptional = {
    let make = () => {
      {kind: UnsupportedRootOptional, path: []}
    }
  }

  module DefaultDestructingFailed = {
    let make = (~destructingErrorMessage) => {
      {
        kind: DefaultDestructingFailed({destructingErrorMessage: destructingErrorMessage}),
        path: [],
      }
    }
  }

  let pathToText = path => {
    switch path {
    | [] => "root"
    | _ => path->Js.Array2.map(pathItem => `["${pathItem}"]`)->Js.Array2.joinWith("")
    }
  }

  let prependLocation = (error, location) => {
    error.path = [location]->Js.Array2.concat(error.path)
    error
  }

  let toString = error => {
    let pathText = error.path->pathToText
    let reason = switch error.kind {
    | UnsupportedRootOptional => `Optional struct is not supported at root`
    | UnsupportedNestedOptional => `Optional struct is not supported inside the Option struct`
    | UnsupportedOptionalItem(structName) =>
      `Optional struct is not supported as ${structName} item`
    | UnsupportedStruct(structName) => `The ${structName} struct is not supported`
    | DefaultDestructingFailed({destructingErrorMessage}) =>
      `Couldn't destruct default value. Error: ${destructingErrorMessage}`
    }
    `[ReScript JSON Schema] Failed converting at ${pathText}. Reason: ${reason}`
  }
}

type t

external unsafeToJsonSchema: 'a => t = "%identity"

module Raw = {
  type t

  external make: 'a => t = "%identity"

  let merge: (t, t) => t = %raw("(s1, s2) => Object.assign({}, s1, s2)")

  let description = value => make({"description": value})

  let default = value => make({"default": value})

  let schemaDialect = make({"$schema": "http://json-schema.org/draft-07/schema#"})

  let empty = make(Js.Dict.empty())

  let string = make({"type": "string"})
  let integer = make({"type": "integer"})
  let number = make({"type": "number"})
  let boolean = make({"type": "boolean"})
  let null = (innerSchema: t) => {
    make({
      "anyOf": [
        innerSchema,
        make({
          "type": "null",
        }),
      ],
    })
  }
  let never = make({"not": Js.Dict.empty()})

  let array = (innerSchema: t) => {
    make({
      "items": innerSchema,
      "type": "array",
    })
  }

  let tuple = items => {
    make({
      "items": items,
      "type": "array",
      "minItems": items->Js.Array2.length,
      "maxItems": items->Js.Array2.length,
    })
  }

  let union = items => {
    make({
      "anyOf": items,
    })
  }

  let dict = (innerSchema: t) => {
    make({
      "type": "object",
      "additionalProperties": innerSchema,
    })
  }

  let record = (
    ~properties: Js.Dict.t<t>,
    ~additionalProperties: bool,
    ~required: array<string>,
  ) => {
    let schema = make({
      "type": "object",
      "properties": properties,
      "additionalProperties": additionalProperties,
    })
    switch required {
    | [] => schema
    | required => merge(schema, make({"required": required}))
    }
  }

  let deprecated: t = make({"deprecated": true})
  let deprecatedWithMessage = (message): t => make({"deprecated": true, "description": message})

  module Literal = {
    let string = value => make({"type": "string", "const": value})
    let integer = value => make({"type": "integer", "const": value})
    let number = value => make({"type": "number", "const": value})
    let boolean = value => make({"type": "boolean", "const": value})
    let null = make({"type": "null"})
  }

  let metadataId: S.Metadata.Id.t<t> = S.Metadata.Id.make(
    ~namespace="rescript-json-schema",
    ~name="raw",
  )
}

type node = {rawSchema: Raw.t, isRequired: bool}

let rec makeNode:
  type value. S.t<value> => result<node, Error.t> =
  struct => {
    let maybeMetadataRawSchema = struct->S.Metadata.get(~id=Raw.metadataId)

    switch struct->S.classify {
    | S.String => Ok({rawSchema: Raw.string, isRequired: true})
    | S.Int => Ok({rawSchema: Raw.integer, isRequired: true})
    | S.Bool => Ok({rawSchema: Raw.boolean, isRequired: true})
    | S.Float => Ok({rawSchema: Raw.number, isRequired: true})
    | S.Array(innerStruct) =>
      makeNode(innerStruct)->Stdlib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.array(innerNode.rawSchema), isRequired: true})
        } else {
          Error(Error.UnsupportedOptionalItem.make(struct))
        }
      })
    | S.Tuple(innerStructs) =>
      innerStructs
      ->Stdlib.Result.Array.mapi((. innerStruct, idx) => {
        makeNode(innerStruct)
        ->Stdlib.Result.flatMap(innerNode => {
          if innerNode.isRequired {
            Ok(innerNode.rawSchema)
          } else {
            Error(Error.UnsupportedOptionalItem.make(struct))
          }
        })
        ->Stdlib.Result.mapError(Error.prependLocation(_, idx->Js.Int.toString))
      })
      ->Stdlib.Result.map(items => {
        {rawSchema: Raw.tuple(items), isRequired: true}
      })
    | S.Union(innerStructs) =>
      innerStructs
      ->Stdlib.Result.Array.mapi((. innerStruct, _) => {
        makeNode(innerStruct)->Stdlib.Result.flatMap(innerNode => {
          if innerNode.isRequired {
            Ok(innerNode.rawSchema)
          } else {
            Error(Error.UnsupportedOptionalItem.make(struct))
          }
        })
      })
      ->Stdlib.Result.map(items => {
        {rawSchema: Raw.union(items), isRequired: true}
      })
    | S.Option(innerStruct) =>
      makeNode(innerStruct)->Stdlib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: innerNode.rawSchema, isRequired: false})
        } else {
          Error(Error.UnsupportedNestedOptional.make())
        }
      })
    | S.Object({fields, fieldNames}) =>
      fieldNames
      ->Stdlib.Result.Array.mapi((. fieldName, _) => {
        let fieldStruct = fields->Js.Dict.unsafeGet(fieldName)
        makeNode(fieldStruct)->Stdlib.Result.mapError(Error.prependLocation(_, fieldName))
      })
      ->Stdlib.Result.map(fieldNodes => {
        let rawSchema = {
          let properties = Js.Dict.empty()
          let required = []
          fieldNodes->Js.Array2.forEachi((fieldNode, idx) => {
            let fieldName = fieldNames->Js.Array2.unsafe_get(idx)
            if fieldNode.isRequired {
              required->Js.Array2.push(fieldName)->ignore
            }
            properties->Js.Dict.set(fieldName, fieldNode.rawSchema)
          })
          Raw.record(
            ~additionalProperties=switch struct->S.Object.UnknownKeys.classify {
            | Strict => false
            | Strip => true
            },
            ~properties,
            ~required,
          )
        }
        {
          rawSchema,
          isRequired: true,
        }
      })
    | S.Unknown => Ok({rawSchema: Raw.empty, isRequired: true})
    | S.Null(innerStruct) =>
      makeNode(innerStruct)->Stdlib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.null(innerNode.rawSchema), isRequired: true})
        } else {
          Error(Error.UnsupportedOptionalItem.make(struct))
        }
      })
    | S.Never => Ok({rawSchema: Raw.never, isRequired: true})
    | S.Literal(Bool(value)) => Ok({rawSchema: Raw.Literal.boolean(value), isRequired: true})
    | S.Literal(Int(value)) => Ok({rawSchema: Raw.Literal.integer(value), isRequired: true})
    | S.Literal(Float(value)) => Ok({rawSchema: Raw.Literal.number(value), isRequired: true})
    | S.Literal(String(value)) => Ok({rawSchema: Raw.Literal.string(value), isRequired: true})
    | S.Literal(EmptyNull) => Ok({rawSchema: Raw.Literal.null, isRequired: true})
    | S.Literal(EmptyOption) => Error(Error.UnsupportedStruct.make(struct))
    | S.Literal(NaN) => Error(Error.UnsupportedStruct.make(struct))
    | S.Dict(innerStruct) =>
      makeNode(innerStruct)->Stdlib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.dict(innerNode.rawSchema), isRequired: true})
        } else {
          Error(Error.UnsupportedOptionalItem.make(struct))
        }
      })
    }
    ->Stdlib.Result.map(node => {
      let rawSchema = switch struct->S.Deprecated.classify {
      | Some(WithoutMessage) => Raw.merge(node.rawSchema, Raw.deprecated)
      | Some(WithMessage(message)) => Raw.merge(node.rawSchema, Raw.deprecatedWithMessage(message))
      | None => node.rawSchema
      }
      {...node, rawSchema}
    })
    ->Stdlib.Result.flatMap(node => {
      switch struct->S.Defaulted.classify {
      | Some(WithDefaultValue(defaultValue)) =>
        switch Some(defaultValue)->Obj.magic->S.serializeWith(struct) {
        | Error(destructingError) =>
          Error(
            Error.DefaultDestructingFailed.make(
              ~destructingErrorMessage=destructingError->S.Error.toString,
            ),
          )
        | Ok(destructedValue) =>
          Ok({
            rawSchema: Raw.merge(node.rawSchema, Raw.default(destructedValue)),
            isRequired: false,
          })
        }
      | None => Ok(node)
      }
    })
    ->Stdlib.Result.map(node => {
      let rawSchema = switch maybeMetadataRawSchema {
      | Some(metadataRawSchema) => Raw.merge(node.rawSchema, metadataRawSchema)
      | None => node.rawSchema
      }
      {...node, rawSchema}
    })
  }

let make = struct => {
  makeNode(struct)
  ->Stdlib.Result.flatMap(node => {
    if node.isRequired {
      Ok(Raw.merge(node.rawSchema, Raw.schemaDialect)->unsafeToJsonSchema)
    } else {
      Error(Error.UnsupportedRootOptional.make())
    }
  })
  ->Stdlib.Result.mapError(Error.toString)
}

let raw = (struct, providedRawSchema) => {
  let rawSchema = switch struct->S.Metadata.get(~id=Raw.metadataId) {
  | Some(existingRawSchema) => Raw.merge(existingRawSchema, providedRawSchema->Raw.make)
  | None => providedRawSchema->Raw.make
  }
  struct->S.Metadata.set(~id=Raw.metadataId, ~metadata=rawSchema)
}

let description = (struct, value) => {
  struct->raw(Raw.description(value))
}
