module Arrayable = {
  type t<'item>
  type tagged<'item> = Single('item) | Array(array<'item>)

  external array: array<'item> => t<'item> = "%identity"
  external single: 'item => t<'item> = "%identity"

  let isArray: t<'item> => bool = Js.Array2.isArray

  let classify = (arrayable: t<'item>): tagged<'item> => {
    if arrayable->isArray {
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
