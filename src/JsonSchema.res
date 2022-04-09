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

type rec branch = {rawSchema: Raw.t, state: branchState}
and branchState = Optional | Required

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
  let make = (~unsafeFieldsArray, ~makeBranch: S.t<'value> => branch): Raw.t => {
    _make(~unsafeFieldsArray, ~makeFieldDetails=struct => {
      let branch = makeBranch(struct)
      {
        schema: branch.rawSchema,
        isRequired: switch branch.state {
        | Required => true
        | Optional => false
        },
      }
    })
  }
}

let rec makeBranch:
  type value. S.t<value> => branch =
  struct => {
    let maybeMetadataRawSchema = struct->Raw.Metadata.extract

    let kindBranch = switch struct->S.classify {
    | S.String => {rawSchema: Raw.string, state: Required}
    | S.Int => {rawSchema: Raw.integer, state: Required}
    | S.Bool => {rawSchema: Raw.boolean, state: Required}
    | S.Float => {rawSchema: Raw.number, state: Required}
    | S.Array(itemStruct) => {
        let itemBranch = makeBranch(itemStruct)
        if itemBranch.state === Optional {
          raise(ArrayItemOptionException)
        }
        {rawSchema: Raw.array(itemBranch.rawSchema), state: Required}
      }
    | S.Option(itemStruct) => {
        let itemBranch = makeBranch(itemStruct)
        if itemBranch.state === Optional {
          raise(NestedOptionException)
        }
        {rawSchema: itemBranch.rawSchema, state: Optional}
      }
    | S.Record(unsafeFieldsArray) => {
        rawSchema: Record.make(~unsafeFieldsArray, ~makeBranch),
        state: Required,
      }
    | S.Custom => {rawSchema: Raw.empty, state: Required}
    }

    switch maybeMetadataRawSchema {
    | Some(metadataRawSchema) => {
        rawSchema: Raw.merge(kindBranch.rawSchema, metadataRawSchema),
        state: kindBranch.state,
      }
    | None => kindBranch
    }
  }

let make = struct => {
  try {
    let branch = makeBranch(struct)
    if branch.state === Optional {
      raise(RootOptionException)
    }
    Raw.merge(branch.rawSchema, Raw.schemaDialect)->unsafeToJsonSchema
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
