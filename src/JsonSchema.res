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

module Arrayable = {
  type t<'item>
  type tagged<'item> = Single('item) | Array(array<'item>)

  external array: array<'item> => t<'item> = "%identity"
  external single: 'item => t<'item> = "%identity"

  let classify = (arrayable: t<'item>): tagged<'item> => {
    if arrayable->Js.Array2.isArray {
      Array(arrayable->(Obj.magic: t<'item> => array<'item>))
    } else {
      Single(arrayable->(Obj.magic: t<'item> => 'item))
    }
  }
}

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
type typeName = [
  | #string
  | #number
  | #integer
  | #boolean
  | #object
  | #array
  | #null
]

/**
 * Meta schema
 *
 * Recommended values:
 * - 'http://json-schema.org/schema#'
 * - 'http://json-schema.org/hyper-schema#'
 * - 'http://json-schema.org/draft-07/schema#'
 * - 'http://json-schema.org/draft-07/hyper-schema#'
 *
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-5
 */
type version = string

/**
 * JSON Schema v7
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
type rec t = {
  @as("$id")
  id?: string,
  @as("$ref")
  ref?: string,
  @as("$schema")
  schema?: version,
  /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
  @as("$defs")
  defs?: Js.Dict.t<definition>,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
  @as("type")
  type_?: Arrayable.t<typeName>,
  enum?: array<Js.Json.t>,
  const?: Js.Json.t,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
  multipleOf?: float,
  maximum?: float,
  exclusiveMaximum?: float,
  minimum?: float,
  exclusiveMinimum?: float,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
  maxLength?: int,
  minLength?: int,
  pattern?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
  items?: Arrayable.t<definition>,
  additionalItems?: definition,
  maxItems?: int,
  minItems?: int,
  uniqueItems?: bool,
  contains?: t,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: int,
  minProperties?: int,
  required?: array<string>,
  properties?: Js.Dict.t<definition>,
  patternProperties?: Js.Dict.t<definition>,
  additionalProperties?: definition,
  dependencies?: Js.Dict.t<dependency>,
  propertyNames?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
  @as("if")
  if_?: definition,
  then?: definition,
  @as("else")
  else_?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
  allOf?: array<definition>,
  anyOf?: array<definition>,
  oneOf?: array<definition>,
  not?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
  format?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
  contentMediaType?: string,
  contentEncoding?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
  definitions?: Js.Dict.t<definition>,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
  title?: string,
  description?: string,
  default?: Js.Json.t,
  readOnly?: bool,
  writeOnly?: bool,
  examples?: Js.Json.t,
}
and definition
and dependency

module Definition = {
  type tagged = Schema(t) | Boolean(bool)

  external schema: t => definition = "%identity"
  external boolean: bool => definition = "%identity"

  let classify = definition => {
    if definition->Js.typeof === "boolean" {
      Boolean(definition->(Obj.magic: definition => bool))
    } else {
      Schema(definition->(Obj.magic: definition => t))
    }
  }
}

module Dependency = {
  type tagged = Schema(t) | Required(array<string>)

  external required: array<string> => dependency = "%identity"
  external schema: t => dependency = "%identity"

  let classify = dependency => {
    if dependency->Js.Array2.isArray {
      Required(dependency->(Obj.magic: dependency => array<string>))
    } else {
      Schema(dependency->(Obj.magic: dependency => t))
    }
  }
}

module Schema = {
  @val
  external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"
  @val
  external mixin: (t, t) => t = "Object.assign"

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

  let deprecated = () => {"deprecated": true}->(Obj.magic: 'a => t)
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

let rawMetadataId: S.Metadata.Id.t<t> = S.Metadata.Id.make(
  ~namespace="rescript-json-schema",
  ~name="raw",
)

type node = {schema: t, isRequired: bool}

let rec makeNode:
  type value. S.t<value> => node =
  struct => {
    let maybeMetadataRawSchema = struct->S.Metadata.get(~id=rawMetadataId)

    switch struct->S.classify {
    | S.String => {schema: Schema.string(), isRequired: true}
    | S.Int => {schema: Schema.integer(), isRequired: true}
    | S.Bool => {schema: Schema.boolean(), isRequired: true}
    | S.Float => {schema: Schema.number(), isRequired: true}
    | S.Array(childStruct) => {
        let childNode = makeNode(childStruct)
        if childNode.isRequired {
          {schema: Schema.array(childNode.schema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }

    | S.Tuple(childStructs) => {
        let items = childStructs->Js.Array2.mapi((childStruct, idx) => {
          let childNode = makeNode(childStruct)
          if childNode.isRequired {
            Definition.schema(childNode.schema)
          } else {
            Error.UnsupportedOptionalItem.raise(~path=[idx->Js.Int.toString], struct)
          }
        })
        {schema: Schema.tuple(items), isRequired: true}
      }

    | S.Union(childStructs) => {
        let items = childStructs->Js.Array2.map(childStruct => {
          let childNode = makeNode(childStruct)
          if childNode.isRequired {
            Definition.schema(childNode.schema)
          } else {
            Error.UnsupportedOptionalItem.raise(struct)
          }
        })
        {schema: Schema.union(items), isRequired: true}
      }

    | S.Option(childStruct) => {
        let childNode = makeNode(childStruct)
        if childNode.isRequired {
          {schema: childNode.schema, isRequired: false}
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
        let schema = {
          let properties = Js.Dict.empty()
          let required = []
          fieldNodes->Js.Array2.forEachi((fieldNode, idx) => {
            let fieldName = fieldNames->Js.Array2.unsafe_get(idx)
            if fieldNode.isRequired {
              required->Js.Array2.push(fieldName)->ignore
            }
            properties->Js.Dict.set(fieldName, Definition.schema(fieldNode.schema))
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
        {
          schema,
          isRequired: true,
        }
      }

    | S.Unknown => {schema: Schema.empty(), isRequired: true}
    | S.Null(childStruct) => {
        let childNode = makeNode(childStruct)
        if childNode.isRequired {
          {schema: Schema.null(childNode.schema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }

    | S.Never => {schema: Schema.never(), isRequired: true}
    | S.Literal(Bool(value)) => {schema: Schema.Literal.boolean(value), isRequired: true}
    | S.Literal(Int(value)) => {schema: Schema.Literal.integer(value), isRequired: true}
    | S.Literal(Float(value)) => {schema: Schema.Literal.number(value), isRequired: true}
    | S.Literal(String(value)) => {schema: Schema.Literal.string(value), isRequired: true}
    | S.Literal(EmptyNull) => {schema: Schema.Literal.null(), isRequired: true}
    | S.Literal(EmptyOption) => Error.UnsupportedStruct.raise(struct)
    | S.Literal(NaN) => Error.UnsupportedStruct.raise(struct)
    | S.Dict(childStruct) => {
        let childNode = makeNode(childStruct)
        if childNode.isRequired {
          {schema: Schema.dict(childNode.schema), isRequired: true}
        } else {
          Error.UnsupportedOptionalItem.raise(struct)
        }
      }
    }->(
      node => {
        let schema = switch struct->S.Deprecated.classify {
        | Some(WithoutMessage) => Schema.mixin(node.schema, Schema.deprecated())
        | Some(WithMessage(message)) =>
          Schema.mixin(node.schema, Schema.deprecatedWithMessage(message))
        | None => node.schema
        }
        let node = {...node, schema}
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
              schema: Schema.mixin(
                node.schema,
                Schema.default(destructedValue->(Obj.magic: unknown => Js.Json.t)),
              ),
              isRequired: false,
            }
          }
        | None => node
        }
        let schema = switch maybeMetadataRawSchema {
        | Some(metadataRawSchema) => Schema.mixin(node.schema, metadataRawSchema)
        | None => node.schema
        }
        {...node, schema}
      }
    )
  }

let make = struct => {
  try {
    let node = makeNode(struct)
    if node.isRequired {
      Ok(Schema.mixin(node.schema, Schema.schemaDialect()))
    } else {
      Error.raise(UnsupportedRootOptional)
    }
  } catch {
  | Error.Exception(error) => Error(error->Error.toString)
  }
}

let raw = (struct, providedRawSchema) => {
  let providedRawSchema = providedRawSchema->(Obj.magic: 'a => t)
  let schema = switch struct->S.Metadata.get(~id=rawMetadataId) {
  | Some(existingRawSchema) => Schema.merge(existingRawSchema, providedRawSchema)
  | None => providedRawSchema
  }
  struct->S.Metadata.set(~id=rawMetadataId, ~metadata=schema)
}

let description = (struct, value) => {
  struct->raw(Schema.description(value))
}
