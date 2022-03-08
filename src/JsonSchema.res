exception NestedOptionException
exception RootOptionException
exception ArrayItemOptionException

let rawSchemaNamespace = "rescript-json-schema:rawSchema"

type rec t
and stateful =
  | Optional(t)
  | Required(t)

external unsafeUnknownToJsonSchema: S.unknown => t = "%identity"

@module
external mergeSchema: (t, t) => t = "deepmerge"

module Raw = {
  module Description = {
    type t = {description: string}
    let make = value => {description: value}
  }
}

module Base = {
  let schemaDialect: t = %raw(`{ $schema: 'http://json-schema.org/draft-07/schema#' }`)

  let string: t = %raw(`{ type: 'string' }`)
  let integer: t = %raw(`{ type: 'integer' }`)
  let number: t = %raw(`{ type: 'number' }`)
  let boolean: t = %raw(`{ type: 'boolean' }`)

  let _array = %raw(`function (itemSchema) {
    return {
      items: itemSchema,
      type: 'array'
    }
  }`)
  let array = (itemSchema: t): t => {
    _array(itemSchema)
  }

  type recordFieldDetails<'value> = {
    schema: t,
    isRequired: bool,
  }
  let _record = %raw(`function(fields, makeFieldDetails) {
    var schema = {
        type: 'object',
        properties: {},
      },
      requiredFieldNames = [];
    fields.forEach(function(field) {
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
  let record = (~fields, ~makeBranch: S.t<'value> => stateful): t => {
    _record(~fields, ~makeFieldDetails=struct => {
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
  type value. S.t<value> => stateful =
  struct => {
    let rawSchema = struct->S.getMeta(~namespace=rawSchemaNamespace)->unsafeUnknownToJsonSchema
    switch struct->S.classify {
    | S.String => Required(Base.string->mergeSchema(rawSchema))
    | S.Int => Required(Base.integer->mergeSchema(rawSchema))
    | S.Bool => Required(Base.boolean->mergeSchema(rawSchema))
    | S.Float => Required(Base.number->mergeSchema(rawSchema))
    | S.Array(s') =>
      Required(
        switch makeBranch(s') {
        | Optional(_) => raise(ArrayItemOptionException)
        | Required(schema) => Base.array(schema)->mergeSchema(rawSchema)
        },
      )
    | S.Option(s') =>
      switch makeBranch(
        s'->S.mixinMeta(~namespace=rawSchemaNamespace, ~meta=rawSchema->S.unsafeToUnknown),
      ) {
      | Optional(_) => raise(NestedOptionException)
      | Required(s'') => Optional(s'')
      }
    | S.Record1(fields) =>
      Required(Base.record(~fields=[fields], ~makeBranch)->mergeSchema(rawSchema))
    | S.Record2(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record3(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record4(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record5(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record6(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record7(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record8(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    | S.Record9(fields) => Required(Base.record(~fields, ~makeBranch)->mergeSchema(rawSchema))
    }
  }

let make = struct => {
  try {
    let schema = switch makeBranch(struct) {
    | Optional(_) => raise(RootOptionException)
    | Required(schema) => schema
    }
    schema->mergeSchema(Base.schemaDialect)
  } catch {
  | NestedOptionException =>
    Js.Exn.raiseError("The option struct can't be nested in another option struct")
  | RootOptionException => Js.Exn.raiseError("The root struct can't be optional")
  | ArrayItemOptionException => Js.Exn.raiseError("Optional array item struct isn't supported")
  // TODO: Raise custom instance of error
  | _ => Js.Exn.raiseError("Unknown RescriptJsonSchema error.")
  }
}

let raw = (struct, schema) => {
  struct->S.mixinMeta(~namespace=rawSchemaNamespace, ~meta=schema->S.unsafeToUnknown)
}

let description = (struct, value) => {
  struct->raw(Raw.Description.make(value))
}
