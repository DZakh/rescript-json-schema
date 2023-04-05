module Error = {
  type rec t = {code: code, mutable path: array<string>}
  and code =
    | UnsupportedNestedOptional
    | UnsupportedRootOptional
    | UnsupportedOptionalItem(string)
    | UnsupportedStruct(string)
    | DefaultDestructingFailed({destructingErrorMessage: string})

  exception Exception(t)

  let raise = code => {
    raise(Exception({code, path: []}))
  }

  module UnsupportedOptionalItem = {
    let raise = struct => {
      raise(UnsupportedOptionalItem(struct->S.name))
    }
  }

  module UnsupportedStruct = {
    let raise = struct => {
      raise(UnsupportedStruct(struct->S.name))
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

include JSONSchema7

module Schema = {
  @val
  external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"
  @val
  external mixin: (t, t) => unit = "Object.assign"

  let empty = (): t => {}
  let description = value => {description: value}
  let default = value => {default: value}
  let schemaDialect = () => {schema: "http://json-schema.org/draft-07/schema#"}
  let string = () => {type_: Arrayable.single(#string)}
  let integer = () => {type_: Arrayable.single(#integer)}
  let number = () => {type_: Arrayable.single(#number)}
  let boolean = () => {type_: Arrayable.single(#boolean)}
  let null = childSchema => {
    {
      anyOf: [
        Definition.schema(childSchema),
        Definition.schema({
          type_: Arrayable.single(#null),
        }),
      ],
    }
  }
  let never = () => {not: Definition.schema({})}

  let array = childSchema => {
    {
      items: Arrayable.single(Definition.schema(childSchema)),
      type_: Arrayable.single(#array),
    }
  }

  let tuple = items => {
    let itemsNumber = items->Js.Array2.length
    {
      items: Arrayable.array(items),
      type_: Arrayable.single(#array),
      minItems: itemsNumber,
      maxItems: itemsNumber,
    }
  }

  let union = items => {
    {
      anyOf: items,
    }
  }

  let dict = childSchema => {
    {
      type_: Arrayable.single(#object),
      additionalProperties: Definition.schema(childSchema),
    }
  }

  let record = (~properties, ~additionalProperties: bool, ~required: array<string>) => {
    let schema = {
      type_: Arrayable.single(#object),
      properties,
      additionalProperties: Definition.boolean(additionalProperties),
    }
    switch required {
    | [] => ()
    | required =>
      (
        schema->(Obj.magic: t => {"required#=": Js_OO.Meth.arity1<array<string> => unit>})
      )["required"] = required
    }
    schema
  }

  let deprecatedWithMessage = message =>
    {"deprecated": true, "description": message}->(Obj.magic: 'a => t)

  module Literal = {
    let string = value => {type_: Arrayable.single(#string), const: Js.Json.string(value)}
    let integer = value => {
      type_: Arrayable.single(#integer),
      const: Js.Json.number(value->Js.Int.toFloat),
    }
    let number = value => {type_: Arrayable.single(#number), const: Js.Json.number(value)}
    let boolean = value => {type_: Arrayable.single(#boolean), const: Js.Json.boolean(value)}
    let null = () => {type_: Arrayable.single(#null)}
  }
}

let schemaExtendMetadataId: S.Metadata.Id.t<t> = S.Metadata.Id.make(
  ~namespace="rescript-json-schema",
  ~name="schemaExtend",
)

let isOptionalStruct = struct =>
  switch struct->S.classify {
  | Option(_) => true
  | _ => false
  }

let rec makeStructSchema:
  type value. S.t<value> => t =
  struct => {
    let schema = switch struct->S.classify {
    | S.String => Schema.string()
    | S.Int => Schema.integer()
    | S.Bool => Schema.boolean()
    | S.Float => Schema.number()
    | S.Array(childStruct) =>
      if childStruct->isOptionalStruct {
        Error.UnsupportedOptionalItem.raise(struct)
      } else {
        Schema.array(makeStructSchema(childStruct))
      }

    | S.Tuple(childStructs) =>
      Schema.tuple(
        childStructs->Js.Array2.mapi((childStruct, idx) => {
          try {
            if childStruct->isOptionalStruct {
              Error.UnsupportedOptionalItem.raise(struct)
            } else {
              Definition.schema(makeStructSchema(childStruct))
            }
          } catch {
          | Error.Exception(error) =>
            raise(Error.Exception(error->Error.prependLocation(idx->Js.Int.toString)))
          }
        }),
      )

    | S.Union(childStructs) =>
      Schema.union(
        childStructs->Js.Array2.map(childStruct => {
          if childStruct->isOptionalStruct {
            Error.UnsupportedOptionalItem.raise(struct)
          } else {
            Definition.schema(makeStructSchema(childStruct))
          }
        }),
      )

    | S.Option(childStruct) =>
      if childStruct->isOptionalStruct {
        Error.raise(UnsupportedNestedOptional)
      } else {
        let schema = makeStructSchema(childStruct)

        switch struct->S.Default.classify {
        | Some(defaultValue) =>
          switch Some(defaultValue)
          ->(Obj.magic: option<unknown> => unknown)
          ->S.serializeWith(childStruct) {
          | Error(destructingError) =>
            Error.raise(
              DefaultDestructingFailed({
                destructingErrorMessage: destructingError->S.Error.toString,
              }),
            )
          | Ok(destructedValue) => schema->Schema.mixin(Schema.default(destructedValue))
          }
        | None => ()
        }

        schema
      }

    | S.Object({fields, fieldNames}) => {
        let properties = Js.Dict.empty()
        let required = []
        fieldNames->Js.Array2.forEach(fieldName => {
          let fieldStruct = fields->Js.Dict.unsafeGet(fieldName)
          let fieldSchema = try {
            makeStructSchema(fieldStruct)
          } catch {
          | Error.Exception(error) =>
            raise(Error.Exception(error->Error.prependLocation(fieldName)))
          }
          if fieldStruct->isOptionalStruct->not {
            required->Js.Array2.push(fieldName)->ignore
          }
          properties->Js.Dict.set(fieldName, Definition.schema(fieldSchema))
        })
        Schema.record(
          ~additionalProperties=switch struct->S.Object.UnknownKeys.classify {
          | Strict => false
          | Strip => true
          },
          ~properties,
          ~required,
        )
      }

    | S.Unknown => Schema.empty()
    | S.Null(childStruct) => Schema.null(makeStructSchema(childStruct))
    | S.Never => Schema.never()
    | S.Literal(Bool(value)) => Schema.Literal.boolean(value)
    | S.Literal(Int(value)) => Schema.Literal.integer(value)
    | S.Literal(Float(value)) => Schema.Literal.number(value)
    | S.Literal(String(value)) => Schema.Literal.string(value)
    | S.Literal(EmptyNull) => Schema.Literal.null()
    | S.Literal(EmptyOption) => Error.UnsupportedStruct.raise(struct)
    | S.Literal(NaN) => Error.UnsupportedStruct.raise(struct)
    | S.Dict(childStruct) =>
      if childStruct->isOptionalStruct {
        Error.UnsupportedOptionalItem.raise(struct)
      } else {
        Schema.dict(makeStructSchema(childStruct))
      }
    }

    switch struct->S.deprecation {
    | Some(message) => schema->Schema.mixin(Schema.deprecatedWithMessage(message))
    | None => ()
    }

    switch struct->S.description {
    | Some(m) => schema->Schema.mixin(Schema.description(m))
    | None => ()
    }

    switch struct->S.Metadata.get(~id=schemaExtendMetadataId) {
    | Some(metadataRawSchema) => schema->Schema.mixin(metadataRawSchema)
    | None => ()
    }

    schema
  }

let make = struct => {
  try {
    if struct->isOptionalStruct {
      Error.raise(UnsupportedRootOptional)
    } else {
      let schema = makeStructSchema(struct)
      schema->Schema.mixin(Schema.schemaDialect())
      Ok(schema)
    }
  } catch {
  | Error.Exception(error) => Error(error->Error.toString)
  }
}

let extend = (struct, schema) => {
  struct->S.Metadata.set(
    ~id=schemaExtendMetadataId,
    ~metadata=switch struct->S.Metadata.get(~id=schemaExtendMetadataId) {
    | Some(existingSchemaExtend) => Schema.merge(existingSchemaExtend, schema)
    | None => schema
    },
  )
}
