module Lib = {
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

type t

external unsafeToJsonSchema: 'a => t = "%identity"

module Raw = {
  type t

  external make: 'a => t = "%identity"

  @module
  external merge: (t, t) => t = "deepmerge"

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

  module Literal = {
    let string = value => make({"type": "string", "const": value})
    let integer = value => make({"type": "integer", "const": value})
    let number = value => make({"type": "number", "const": value})
    let boolean = value => make({"type": "boolean", "const": value})
    let null = make({"type": "null"})
  }

  module Metadata = S.MakeMetadata({
    type content = t
    let namespace = "rescript-json-schema:raw"
  })
}

type node = {rawSchema: Raw.t, isRequired: bool}

let rec makeNode:
  type value. S.t<value> => result<node, JsonSchema_Error.t> =
  struct => {
    let maybeMetadataRawSchema = struct->Raw.Metadata.get

    switch struct->S.classify {
    | S.String => Ok({rawSchema: Raw.string, isRequired: true})
    | S.Int => Ok({rawSchema: Raw.integer, isRequired: true})
    | S.Bool => Ok({rawSchema: Raw.boolean, isRequired: true})
    | S.Float => Ok({rawSchema: Raw.number, isRequired: true})
    | S.Array(innerStruct) =>
      makeNode(innerStruct)->Lib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.array(innerNode.rawSchema), isRequired: true})
        } else {
          Error(JsonSchema_Error.UnsupportedOptionalDictItem.make())
        }
      })
    | S.Tuple(innerStructs) =>
      innerStructs
      ->Lib.Result.Array.mapi((. innerStruct, idx) => {
        makeNode(innerStruct)
        ->Lib.Result.flatMap(innerNode => {
          if innerNode.isRequired {
            Ok(innerNode.rawSchema)
          } else {
            Error(JsonSchema_Error.UnsupportedOptionalDictItem.make())
          }
        })
        ->Lib.Result.mapError(JsonSchema_Error.prependField(_, idx->Js.Int.toString))
      })
      ->Lib.Result.map(items => {
        {rawSchema: Raw.tuple(items), isRequired: true}
      })
    | S.Union(innerStructs) =>
      innerStructs
      ->Lib.Result.Array.mapi((. innerStruct, idx) => {
        makeNode(innerStruct)
        ->Lib.Result.flatMap(innerNode => {
          if innerNode.isRequired {
            Ok(innerNode.rawSchema)
          } else {
            Error(JsonSchema_Error.UnsupportedOptionalUnionItem.make())
          }
        })
        ->Lib.Result.mapError(JsonSchema_Error.prependField(_, idx->Js.Int.toString))
      })
      ->Lib.Result.map(items => {
        {rawSchema: Raw.union(items), isRequired: true}
      })
    | S.Instance(_) => Error(JsonSchema_Error.UnsupportedInstance.make())
    | S.Option(innerStruct) =>
      makeNode(innerStruct)->Lib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: innerNode.rawSchema, isRequired: false})
        } else {
          Error(JsonSchema_Error.UnsupportedNestedOptional.make())
        }
      })
    | S.Record({fields, fieldNames, unknownKeys}) =>
      fieldNames
      ->Lib.Result.Array.mapi((. fieldName, _) => {
        let fieldStruct = fields->Js.Dict.unsafeGet(fieldName)
        makeNode(fieldStruct)->Lib.Result.mapError(JsonSchema_Error.prependField(_, fieldName))
      })
      ->Lib.Result.map(fieldNodes => {
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
            ~additionalProperties=switch unknownKeys {
            | Strict => false
            | Strip => true
            },
            ~properties,
            ~required,
          )
        }
        {
          rawSchema: rawSchema,
          isRequired: true,
        }
      })
    | S.Unknown => Ok({rawSchema: Raw.empty, isRequired: true})
    | S.Null(innerStruct) =>
      makeNode(innerStruct)->Lib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.null(innerNode.rawSchema), isRequired: true})
        } else {
          Error(JsonSchema_Error.UnsupportedOptionalNullItem.make())
        }
      })
    | S.Never => Ok({rawSchema: Raw.never, isRequired: true})
    | S.Literal(S.Bool(value)) => Ok({rawSchema: Raw.Literal.boolean(value), isRequired: true})
    | S.Literal(S.Int(value)) => Ok({rawSchema: Raw.Literal.integer(value), isRequired: true})
    | S.Literal(S.Float(value)) => Ok({rawSchema: Raw.Literal.number(value), isRequired: true})
    | S.Literal(S.String(value)) => Ok({rawSchema: Raw.Literal.string(value), isRequired: true})
    | S.Literal(S.EmptyNull) => Ok({rawSchema: Raw.Literal.null, isRequired: true})
    | S.Literal(S.EmptyOption) => Error(JsonSchema_Error.UnsupportedEmptyOptionLiteral.make())
    | S.Literal(S.NaN) => Error(JsonSchema_Error.UnsupportedNaNLiteral.make())
    | S.Dict(innerStruct) =>
      makeNode(innerStruct)->Lib.Result.flatMap(innerNode => {
        if innerNode.isRequired {
          Ok({rawSchema: Raw.dict(innerNode.rawSchema), isRequired: true})
        } else {
          Error(JsonSchema_Error.UnsupportedOptionalDictItem.make())
        }
      })
    | S.Deprecated({struct: innerStruct, maybeMessage}) =>
      makeNode(innerStruct)->Lib.Result.flatMap(innerNode => {
        let rawSchema = {
          let rawSchema' = Raw.merge(innerNode.rawSchema, Raw.deprecated)
          switch maybeMessage {
          | Some(message) => Raw.merge(rawSchema', Raw.description(message))
          | None => rawSchema'
          }
        }
        Ok({rawSchema: rawSchema, isRequired: false})
      })
    | S.Default({struct: innerStruct, value}) =>
      switch Some(value)->S.serializeWith(innerStruct) {
      | Error(destructingError) =>
        Error(
          JsonSchema_Error.DefaultDestructingFailed.make(
            ~destructingErrorMessage=destructingError->S.Error.toString,
          ),
        )
      | Ok(destructedValue) =>
        makeNode(innerStruct)->Lib.Result.map(innerNode => {
          {
            rawSchema: Raw.merge(innerNode.rawSchema, Raw.default(destructedValue)),
            isRequired: false,
          }
        })
      }
    }->Lib.Result.map(node => {
      switch maybeMetadataRawSchema {
      | Some(metadataRawSchema) => {
          rawSchema: Raw.merge(node.rawSchema, metadataRawSchema),
          isRequired: node.isRequired,
        }
      | None => node
      }
    })
  }

let make = struct => {
  makeNode(struct)
  ->Lib.Result.flatMap(node => {
    if node.isRequired {
      Ok(Raw.merge(node.rawSchema, Raw.schemaDialect)->unsafeToJsonSchema)
    } else {
      Error(JsonSchema_Error.UnsupportedRootOptional.make())
    }
  })
  ->Lib.Result.mapError(JsonSchema_Error.toString)
}

let raw = (struct, providedRawSchema) => {
  let rawSchema = switch struct->Raw.Metadata.get {
  | Some(existingRawSchema) => Raw.merge(existingRawSchema, providedRawSchema->Raw.make)
  | None => providedRawSchema->Raw.make
  }
  struct->Raw.Metadata.set(rawSchema)
}

let description = (struct, value) => {
  struct->raw(Raw.description(value))
}
