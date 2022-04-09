exception NestedOptionException
exception RootOptionException
exception ArrayItemOptionException

type t

external unsafeToJsonSchema: 'a => t = "%identity"

module Raw = {
  type t

  external make: 'a => t = "%identity"

  @module
  external merge: (t, t) => t = "deepmerge"

  let description = value => make({"description": value})

  let schemaDialect = make({"$schema": "http://json-schema.org/draft-07/schema#"})

  let empty = make(Js.Dict.empty())

  let string = make({"type": "string"})
  let integer = make({"type": "integer"})
  let number = make({"type": "number"})
  let boolean = make({"type": "boolean"})

  let array = (itemSchema: t) => {
    make({
      "items": itemSchema,
      "type": "array",
    })
  }

  module Metadata = S.MakeMetadata({
    type content = t
    let namespace = "rescript-json-schema:raw"
  })
}

type schemaBranch =
  | Optional(Raw.t)
  | Required(Raw.t)

module Record = {
  type recordFieldDetails<'value> = {
    schema: Raw.t,
    isRequired: bool,
  }
  let _make = %raw(`function(unsafeFieldsArray, makeFieldDetails) {
    var schema = {
        type: 'object',
        properties: {},
      },
      requiredFieldNames = [];
    unsafeFieldsArray.forEach(function(field) {
      var fieldName = field[0],
        fieldStruct = field[1],
        fieldDetails = makeFieldDetails(fieldStruct);
      if (fieldDetails.isRequired) {
        if (!schema.required) {
          schema.required = [];
        }
        schema.required.push(fieldName);
      }
      schema.properties[fieldName] = fieldDetails.schema;
    })
    return schema;
  }`)
  let make = (~unsafeFieldsArray, ~makeBranch: S.t<'value> => schemaBranch): Raw.t => {
    _make(~unsafeFieldsArray, ~makeFieldDetails=struct => {
      switch makeBranch(struct) {
      | Optional(schema) => {schema: schema, isRequired: false}
      | Required(schema) => {
          schema: schema,
          isRequired: true,
        }
      }
    })
  }
}

let rec makeBranch:
  type value. S.t<value> => schemaBranch =
  struct => {
    let rawSchema = struct->Raw.Metadata.extract->Belt.Option.getWithDefault(Raw.empty)

    switch struct->S.classify {
    | S.String => Required(Raw.merge(Raw.string, rawSchema))
    | S.Int => Required(Raw.merge(Raw.integer, rawSchema))
    | S.Bool => Required(Raw.merge(Raw.boolean, rawSchema))
    | S.Float => Required(Raw.merge(Raw.number, rawSchema))
    | S.Array(struct') =>
      Required(
        switch makeBranch(struct') {
        | Optional(_) => raise(ArrayItemOptionException)
        | Required(rawSchema') => Raw.merge(Raw.array(rawSchema'), rawSchema)
        },
      )
    | S.Option(struct') =>
      switch makeBranch(struct'->Raw.Metadata.mixin(rawSchema)) {
      | Optional(_) => raise(NestedOptionException)
      | Required(struct'') => Optional(struct'')
      }
    | S.Record(unsafeFieldsArray) =>
      Required(Raw.merge(Record.make(~unsafeFieldsArray, ~makeBranch), rawSchema))
    }
  }

let make = struct => {
  try {
    let rawSchema = switch makeBranch(struct) {
    | Optional(_) => raise(RootOptionException)
    | Required(rawSchema') => rawSchema'
    }
    Raw.merge(rawSchema, Raw.schemaDialect)->unsafeToJsonSchema
  } catch {
  | NestedOptionException =>
    Js.Exn.raiseError("The option struct can't be nested in another option struct")
  | RootOptionException => Js.Exn.raiseError("The root struct can't be optional")
  | ArrayItemOptionException => Js.Exn.raiseError("Optional array item struct isn't supported")
  // TODO: Raise custom instance of error
  | _ => Js.Exn.raiseError("Unknown RescriptJsonSchema error.")
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
