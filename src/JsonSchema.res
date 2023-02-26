module Error = {
  type rec t = {code: code, mutable path: array<string>}
  and code =
    | UnsupportedNestedOptional
    | UnsupportedRootOptional
    | UnsupportedOptionalItem(string)
    | UnsupportedStruct(string)
    | DefaultDestructingFailed({destructingErrorMessage: string})

  exception Exception(t)

  let raise = (~path=[], code) => {
    raise(Exception({code, path}))
  }

  module UnsupportedOptionalItem = {
    let raise = (~path=?, struct) => {
      raise(~path?, UnsupportedOptionalItem(struct->S.name))
    }
  }

  module UnsupportedStruct = {
    let raise = (~path=?, struct) => {
      raise(~path?, UnsupportedStruct(struct->S.name))
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
    let reason = switch error.code {
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

  let mixin: (t, t) => t = %raw("(s1, s2) => Object.assign(s1, s2)")

  let description = value => make({"description": value})

  let default = value => make({"default": value})

  let schemaDialect = () => make({"$schema": "http://json-schema.org/draft-07/schema#"})

  let empty = () => make(Js.Dict.empty())

  let string = () => make({"type": "string"})
  let integer = () => make({"type": "integer"})
  let number = () => make({"type": "number"})
  let boolean = () => make({"type": "boolean"})
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
  let never = () => make({"not": Js.Dict.empty()})

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
    | required => mixin(schema, make({"required": required}))
    }
  }

  let deprecated = () => make({"deprecated": true})
  let deprecatedWithMessage = message => make({"deprecated": true, "description": message})

  module Literal = {
    let string = value => make({"type": "string", "const": value})
    let integer = value => make({"type": "integer", "const": value})
    let number = value => make({"type": "number", "const": value})
    let boolean = value => make({"type": "boolean", "const": value})
    let null = () => make({"type": "null"})
  }

  let metadataId: S.Metadata.Id.t<t> = S.Metadata.Id.make(
    ~namespace="rescript-json-schema",
    ~name="raw",
  )
}

type node = {rawSchema: Raw.t, isRequired: bool}

let rec makeNode:
  type value. S.t<value> => node =
  struct => {
    let maybeMetadataRawSchema = struct->S.Metadata.get(~id=Raw.metadataId)

    switch struct->S.classify {
    | S.String => {rawSchema: Raw.string(), isRequired: true}
    | S.Int => {rawSchema: Raw.integer(), isRequired: true}
    | S.Bool => {rawSchema: Raw.boolean(), isRequired: true}
    | S.Float => {rawSchema: Raw.number(), isRequired: true}
    | S.Array(innerStruct) => {
        let innerNode = makeNode(innerStruct)
        if innerNode.isRequired {
          {rawSchema: Raw.array(innerNode.rawSchema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }

    | S.Tuple(innerStructs) => {
        let items = innerStructs->Js.Array2.mapi((innerStruct, idx) => {
          let innerNode = makeNode(innerStruct)
          if innerNode.isRequired {
            innerNode.rawSchema
          } else {
            Error.UnsupportedOptionalItem.raise(~path=[idx->Js.Int.toString], struct)
          }
        })
        {rawSchema: Raw.tuple(items), isRequired: true}
      }

    | S.Union(innerStructs) => {
        let items = innerStructs->Js.Array2.map(innerStruct => {
          let innerNode = makeNode(innerStruct)
          if innerNode.isRequired {
            innerNode.rawSchema
          } else {
            Error.UnsupportedOptionalItem.raise(struct)
          }
        })
        {rawSchema: Raw.union(items), isRequired: true}
      }

    | S.Option(innerStruct) => {
        let innerNode = makeNode(innerStruct)
        if innerNode.isRequired {
          {rawSchema: innerNode.rawSchema, isRequired: false}
        } else {
          Error.raise(UnsupportedNestedOptional)
        }
      }

    | S.Object({fields, fieldNames}) => {
        let fieldNodes = fieldNames->Js.Array2.map(fieldName => {
          let fieldStruct = fields->Js.Dict.unsafeGet(fieldName)
          try {
            makeNode(fieldStruct)
          } catch {
          | Error.Exception(error) =>
            raise(Error.Exception(error->Error.prependLocation(fieldName)))
          }
        })
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
      }

    | S.Unknown => {rawSchema: Raw.empty(), isRequired: true}
    | S.Null(innerStruct) => {
        let innerNode = makeNode(innerStruct)
        if innerNode.isRequired {
          {rawSchema: Raw.null(innerNode.rawSchema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }

    | S.Never => {rawSchema: Raw.never(), isRequired: true}
    | S.Literal(Bool(value)) => {rawSchema: Raw.Literal.boolean(value), isRequired: true}
    | S.Literal(Int(value)) => {rawSchema: Raw.Literal.integer(value), isRequired: true}
    | S.Literal(Float(value)) => {rawSchema: Raw.Literal.number(value), isRequired: true}
    | S.Literal(String(value)) => {rawSchema: Raw.Literal.string(value), isRequired: true}
    | S.Literal(EmptyNull) => {rawSchema: Raw.Literal.null(), isRequired: true}
    | S.Literal(EmptyOption) => Error.UnsupportedStruct.raise(struct)
    | S.Literal(NaN) => Error.UnsupportedStruct.raise(struct)
    | S.Dict(innerStruct) => {
        let innerNode = makeNode(innerStruct)
        if innerNode.isRequired {
          {rawSchema: Raw.dict(innerNode.rawSchema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }
    }->(
      node => {
        let rawSchema = switch struct->S.Deprecated.classify {
        | Some(WithoutMessage) => Raw.mixin(node.rawSchema, Raw.deprecated())
        | Some(WithMessage(message)) =>
          Raw.mixin(node.rawSchema, Raw.deprecatedWithMessage(message))
        | None => node.rawSchema
        }
        let node = {...node, rawSchema}
        let node = switch struct->S.Defaulted.classify {
        | Some(WithDefaultValue(defaultValue)) =>
          switch Some(defaultValue)
          ->(Obj.magic: option<unknown> => value)
          ->S.serializeWith(struct) {
          | Error(destructingError) =>
            Error.raise(
              DefaultDestructingFailed({
                destructingErrorMessage: destructingError->S.Error.toString,
              }),
            )
          | Ok(destructedValue) => {
              rawSchema: Raw.mixin(node.rawSchema, Raw.default(destructedValue)),
              isRequired: false,
            }
          }
        | None => node
        }
        let rawSchema = switch maybeMetadataRawSchema {
        | Some(metadataRawSchema) => Raw.mixin(node.rawSchema, metadataRawSchema)
        | None => node.rawSchema
        }
        {...node, rawSchema}
      }
    )
  }

let make = struct => {
  try {
    let node = makeNode(struct)
    if node.isRequired {
      Ok(Raw.mixin(node.rawSchema, Raw.schemaDialect())->unsafeToJsonSchema)
    } else {
      Error.raise(UnsupportedRootOptional)
    }
  } catch {
  | Error.Exception(error) => Error(error->Error.toString)
  }
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
