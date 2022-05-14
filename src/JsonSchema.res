exception NestedOptionException
exception RootOptionException
exception ArrayItemOptionException
exception DictItemOptionException
exception DefaultValueException

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

  let deprecated: t = make({"deprecated": true})

  module Metadata = S.MakeMetadata({
    type content = t
    let namespace = "rescript-json-schema:raw"
  })
}

type node = {rawSchema: Raw.t, isRequired: bool}

module Record = {
  let _make = %raw(`function(unsafeFieldsArray, makeChildNode) {
    var schema = {
        type: 'object',
        properties: {},
        additionalProperties: false,
      },
      requiredFieldNames = [];
    unsafeFieldsArray.forEach(function(field) {
      var fieldName = field[0],
        fieldStruct = field[1],
        fieldDetails = makeChildNode(fieldStruct);
      if (fieldDetails.isRequired) {
        if (!schema.required) {
          schema.required = [];
        }
        schema.required.push(fieldName);
      }
      schema.properties[fieldName] = fieldDetails.rawSchema;
    })
    return schema;
  }`)
  let make = (~unsafeFieldsArray, ~makeNode: S.t<'value> => node): Raw.t => {
    _make(~unsafeFieldsArray, ~makeChildNode=makeNode)
  }
}

let rec makeNode:
  type value. S.t<value> => node =
  struct => {
    let maybeMetadataRawSchema = struct->Raw.Metadata.extract

    let node = switch struct->S.classify {
    | S.String => {rawSchema: Raw.string, isRequired: true}
    | S.Int => {rawSchema: Raw.integer, isRequired: true}
    | S.Bool => {rawSchema: Raw.boolean, isRequired: true}
    | S.Float => {rawSchema: Raw.number, isRequired: true}
    | S.Array(childStruct) => {
        let childNode = makeNode(childStruct)
        if !childNode.isRequired {
          raise(ArrayItemOptionException)
        }
        {rawSchema: Raw.array(childNode.rawSchema), isRequired: true}
      }
    | S.Option(childStruct) => {
        let childNode = makeNode(childStruct)
        if !childNode.isRequired {
          raise(NestedOptionException)
        }
        {rawSchema: childNode.rawSchema, isRequired: false}
      }
    | S.Record(unsafeFieldsArray) => {
        rawSchema: Record.make(~unsafeFieldsArray, ~makeNode),
        isRequired: true,
      }
    | S.Unknown => {rawSchema: Raw.empty, isRequired: true}
    | S.Null(_) => Js.Exn.raiseError("The Null struct isn't supported yet")
    | S.Dict(childStruct) => {
        let childNode = makeNode(childStruct)
        if !childNode.isRequired {
          raise(DictItemOptionException)
        }
        {rawSchema: Raw.dict(childNode.rawSchema), isRequired: true}
      }
    | S.Deprecated({struct: childStruct, maybeMessage}) => {
        let childNode = makeNode(childStruct)
        let rawSchema = {
          let rawSchema' = Raw.merge(childNode.rawSchema, Raw.deprecated)
          switch maybeMessage {
          | Some(message) => Raw.merge(rawSchema', Raw.description(message))
          | None => rawSchema'
          }
        }
        {rawSchema: rawSchema, isRequired: false}
      }
    | S.Default({struct: childStruct, value}) =>
      switch Some(value)->S.destructWith(childStruct) {
      | Error(_) => raise(DefaultValueException)
      | Ok(destructedValue) => {
          let childNode = makeNode(childStruct)
          {
            rawSchema: Raw.merge(childNode.rawSchema, Raw.default(destructedValue)),
            isRequired: false,
          }
        }
      }
    }

    switch maybeMetadataRawSchema {
    | Some(metadataRawSchema) => {
        rawSchema: Raw.merge(node.rawSchema, metadataRawSchema),
        isRequired: node.isRequired,
      }
    | None => node
    }
  }

let make = struct => {
  try {
    let node = makeNode(struct)
    if !node.isRequired {
      raise(RootOptionException)
    }
    Ok(Raw.merge(node.rawSchema, Raw.schemaDialect)->unsafeToJsonSchema)
  } catch {
  | NestedOptionException => Error("The option struct can't be nested in another option struct")
  | RootOptionException => Error("The root struct can't be optional")
  | ArrayItemOptionException => Error("Optional array item struct isn't supported")
  | DictItemOptionException => Error("Optional dict item struct isn't supported")
  | DefaultValueException => Error("Couldn't destruct value for default")
  // TODO: Raise custom instance of error
  | _ => Error("Unknown RescriptJsonSchema error.")
  }
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
