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

  let array = (childSchema: t) => {
    make({
      "items": childSchema,
      "type": "array",
    })
  }

  let dict = (childSchema: t) => {
    make({
      "type": "object",
      "additionalProperties": childSchema,
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

  module Metadata = S.MakeMetadata({
    type content = t
    let namespace = "rescript-json-schema:raw"
  })
}

type node = {rawSchema: Raw.t, isRequired: bool}

let rec makeNode:
  type value. S.t<value> => result<node, string> =
  struct => {
    let maybeMetadataRawSchema = struct->Raw.Metadata.extract

    switch struct->S.classify {
    | S.String => Ok({rawSchema: Raw.string, isRequired: true})
    | S.Int => Ok({rawSchema: Raw.integer, isRequired: true})
    | S.Bool => Ok({rawSchema: Raw.boolean, isRequired: true})
    | S.Float => Ok({rawSchema: Raw.number, isRequired: true})
    | S.Array(childStruct) =>
      makeNode(childStruct)->Belt.Result.flatMap(childNode => {
        if childNode.isRequired {
          Ok({rawSchema: Raw.array(childNode.rawSchema), isRequired: true})
        } else {
          Error("Optional array item struct isn't supported")
        }
      })
    | S.Option(childStruct) =>
      makeNode(childStruct)->Belt.Result.flatMap(childNode => {
        if childNode.isRequired {
          Ok({rawSchema: childNode.rawSchema, isRequired: false})
        } else {
          Error("The option struct can't be nested in another option struct")
        }
      })
    | S.Record(fields) =>
      fields
      ->RescriptStruct_ResultX.Array.mapi((field, _) => {
        let (_, fieldStruct) = field
        makeNode(fieldStruct)
      })
      ->Belt.Result.map(fieldNodes => {
        let rawSchema = {
          let properties = Js.Dict.empty()
          let required = []
          fieldNodes->Js.Array2.forEachi((fieldNode, idx) => {
            let field = fields->Js.Array2.unsafe_get(idx)
            let (fieldName, _) = field
            if fieldNode.isRequired {
              required->Js.Array2.push(fieldName)->ignore
            }
            properties->Js.Dict.set(fieldName, fieldNode.rawSchema)
          })
          Raw.record(~additionalProperties=false, ~properties, ~required)
        }
        {
          rawSchema: rawSchema,
          isRequired: true,
        }
      })
    | S.Unknown => Ok({rawSchema: Raw.empty, isRequired: true})
    | S.Null(_) => Js.Exn.raiseError("The Null struct isn't supported yet")
    | S.Dict(childStruct) =>
      makeNode(childStruct)->Belt.Result.flatMap(childNode => {
        if childNode.isRequired {
          Ok({rawSchema: Raw.dict(childNode.rawSchema), isRequired: true})
        } else {
          Error("Optional dict item struct isn't supported")
        }
      })
    | S.Deprecated({struct: childStruct, maybeMessage}) =>
      makeNode(childStruct)->Belt.Result.flatMap(childNode => {
        let rawSchema = {
          let rawSchema' = Raw.merge(childNode.rawSchema, Raw.deprecated)
          switch maybeMessage {
          | Some(message) => Raw.merge(rawSchema', Raw.description(message))
          | None => rawSchema'
          }
        }
        Ok({rawSchema: rawSchema, isRequired: false})
      })
    | S.Default({struct: childStruct, value}) =>
      switch Some(value)->S.destructWith(childStruct) {
      | Error(_) => Error("Couldn't destruct value for default")
      | Ok(destructedValue) =>
        makeNode(childStruct)->Belt.Result.map(childNode => {
          {
            rawSchema: Raw.merge(childNode.rawSchema, Raw.default(destructedValue)),
            isRequired: false,
          }
        })
      }
    }->Belt.Result.map(node => {
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
  makeNode(struct)->Belt.Result.flatMap(node => {
    if node.isRequired {
      Ok(Raw.merge(node.rawSchema, Raw.schemaDialect)->unsafeToJsonSchema)
    } else {
      Error("The root struct can't be optional")
    }
  })
}

let raw = (struct, providedRawSchema) => {
  let rawSchema = switch struct->Raw.Metadata.extract {
  | Some(existingRawSchema) => Raw.merge(existingRawSchema, providedRawSchema)
  | None => providedRawSchema
  }
  struct->Raw.Metadata.mixin(rawSchema)
}

let description = (struct, value) => {
  struct->raw(Raw.description(value))
}
