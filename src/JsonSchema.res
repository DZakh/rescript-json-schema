module Deepmerge = {
  @module
  external merge: ('a, 'b) => 'c = "deepmerge"
}

exception NestedOptionException
exception RootOptionException
exception ArrayItemOptionException

type rec t<'value>
and stateful<'value> =
  | Optional(t<'value>)
  | Required(t<option<'value>>)

external unsafeSetRequired: t<option<'value>> => t<'value> = "%identity"

module Raw = {
  let schemaDialect = %raw(`{ $schema: 'http://json-schema.org/draft-07/schema#' }`)

  let string: t<option<string>> = %raw(`{ type: 'string' }`)
  let integer: t<option<int>> = %raw(`{ type: 'integer' }`)
  let number: t<option<float>> = %raw(`{ type: 'number' }`)
  let boolean: t<option<bool>> = %raw(`{ type: 'boolean' }`)

  let _array = %raw(`function (itemSchema) {
    return {
      items: itemSchema,
      type: 'array'
    }
  }`)
  let array = (itemSchema: t<'item>): t<option<array<'item>>> => {
    _array(itemSchema)
  }

  type recordFieldDetails<'value> = {
    schema: t<'value>,
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
  let record = (~fields, ~makeBranch: S.t<'value> => stateful<'value>) => {
    _record(~fields, ~makeFieldDetails=struct => {
      switch makeBranch(struct) {
      | Optional(schema) => {schema: schema, isRequired: false}
      | Required(schema) => {
          schema: schema->unsafeSetRequired,
          isRequired: true,
        }
      }
    })
  }
}

let rec makeBranch:
  type value. S.t<value> => stateful<value> =
  struct => {
    switch struct->S.classify {
    | S.String => Required(Raw.string)
    | S.Int => Required(Raw.integer)
    | S.Bool => Required(Raw.boolean)
    | S.Float => Required(Raw.number)
    | S.Array(s') =>
      Required(
        switch makeBranch(s') {
        | Optional(_) => raise(ArrayItemOptionException)
        | Required(schema) => Raw.array(schema->unsafeSetRequired)
        },
      )
    | S.Option(s') =>
      switch makeBranch(s') {
      | Optional(_) => raise(NestedOptionException)
      | Required(s'') => Optional(s'')
      }
    | S.Record1(fields) => Required(Raw.record(~fields=[fields], ~makeBranch))
    | S.Record2(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record3(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record4(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record5(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record6(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record7(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record8(fields) => Required(Raw.record(~fields, ~makeBranch))
    | S.Record9(fields) => Required(Raw.record(~fields, ~makeBranch))
    }
  }

let make = struct => {
  try {
    let schema = switch makeBranch(struct) {
    | Optional(_) => raise(RootOptionException)
    | Required(schema) => schema->unsafeSetRequired
    }
    Deepmerge.merge(Raw.schemaDialect, schema)
  } catch {
  | NestedOptionException =>
    Js.Exn.raiseError("The option struct can't be nested in another option struct")
  | RootOptionException => Js.Exn.raiseError("The root struct can't be optional")
  | ArrayItemOptionException => Js.Exn.raiseError("Optional array item struct isn't supported")
  // TODO: Raise custom instance of error
  | _ => Js.Exn.raiseError("Unknown RescriptJsonSchema error.")
  }
}
