module FJS = {
  type t<'v>
  type json<'v>
  @module("fluent-json-schema")
  external object: unit => t<option<'v>> = "object"
  @module("fluent-json-schema")
  external string: unit => t<option<string>> = "string"
  @module("fluent-json-schema")
  external integer: unit => t<option<int>> = "integer"
  @module("fluent-json-schema")
  external boolean: unit => t<option<bool>> = "boolean"
  @module("fluent-json-schema")
  external number: unit => t<option<float>> = "number"
  @module("fluent-json-schema")
  external array: unit => t<option<array<'item>>> = "array"

  @send external items: (t<option<array<'item>>>, t<'item>) => t<option<array<'item>>> = "items"
  @send external valueOf: t<'v> => json<'v> = "valueOf"
}

exception NestedOptionException
exception RootOptionException

type t<'value> = FJS.json<'value>
type rec fluentSchema<'value> = FJS.t<'value>
and meta<'value> =
  | Optional(fluentSchema<'value>)
  | Required(fluentSchema<option<'value>>)

external unsafeUnwrapFluentSchema: fluentSchema<option<'value>> => fluentSchema<'value> =
  "%identity"

let applyFluentSchemaMeta = (meta: meta<'value>): fluentSchema<'value> => {
  switch meta {
  | Optional(_) => raise(RootOptionException)
  | Required(fluentSchema) => fluentSchema->unsafeUnwrapFluentSchema
  }
}

module RecordFluentSchema = {
  type fieldSchema<'value> = {
    fluentSchema: fluentSchema<'value>,
    isRequired: bool,
  }

  let _make = %raw(`
function(fields, makeFieldSchema, objectFluentSchema) {
  var requiredFieldNames = [];
  fields.forEach(function(field) {
    var fieldName = field[0],
      fieldStruct = field[1],
      fieldSchema = makeFieldSchema(fieldStruct);
    if (fieldSchema.isRequired) {
      requiredFieldNames.push(fieldName);
    }
    objectFluentSchema = objectFluentSchema.prop(fieldName, fieldSchema.fluentSchema);
  })
  return objectFluentSchema.required(requiredFieldNames);
}`)

  let make = (~fields, ~makeFluentSchemaWithMeta: S.struct<'value> => meta<'value>) => {
    _make(
      ~fields,
      ~makeFieldSchema=struct => {
        switch makeFluentSchemaWithMeta(struct) {
        | Optional(fluentSchema) => {fluentSchema: fluentSchema, isRequired: false}
        | Required(fluentSchema) => {
            fluentSchema: fluentSchema->unsafeUnwrapFluentSchema,
            isRequired: true,
          }
        }
      },
      ~objectFluentSchema=FJS.object(),
    )
  }
}

let rec makeFluentSchemaWithMeta:
  type value. S.struct<value> => meta<value> =
  struct => {
    switch struct->S.classify {
    | S.String => Required(FJS.string())
    | S.Int => Required(FJS.integer())
    | S.Bool => Required(FJS.boolean())
    | S.Float => Required(FJS.number())
    | S.Array(s') =>
      Required(FJS.array()->FJS.items(makeFluentSchemaWithMeta(s')->applyFluentSchemaMeta))
    | S.Option(s') =>
      switch makeFluentSchemaWithMeta(s') {
      | Optional(_) => raise(NestedOptionException)
      | Required(s'') => Optional(s'')
      }
    | S.Record1(fields) =>
      Required(RecordFluentSchema.make(~fields=[fields], ~makeFluentSchemaWithMeta))
    | S.Record2(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record3(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record4(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record5(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record6(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record7(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record8(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    | S.Record9(fields) => Required(RecordFluentSchema.make(~fields, ~makeFluentSchemaWithMeta))
    }
  }

let make = struct => {
  try {
    makeFluentSchemaWithMeta(struct)->applyFluentSchemaMeta->FJS.valueOf
  } catch {
  | NestedOptionException =>
    Js.Exn.raiseError("The option struct can't be nested in another option struct.")
  | RootOptionException => Js.Exn.raiseError("The root struct can't be optional.")
  // TODO: Handle FluentSchema error
  // TODO: Raise custom instance of error
  | _ => Js.Exn.raiseError("Unknown RescriptJsonSchema error.")
  }
}
