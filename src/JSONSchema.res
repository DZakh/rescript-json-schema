@@uncurried

%%private(external magic: 'a => 'b = "%identity")

module Error = {
  type rec t = {code: code, mutable path: array<string>}
  and code =
    | UnsupportedNestedOptional
    | UnsupportedRootOptional
    | UnsupportedOptionalItem(string)
    | UnsupportedSchema(string)
    | DefaultDestructingFailed({destructingErrorMessage: string})

  exception Exception(t)

  let raise = code => {
    raise(Exception({code, path: []}))
  }

  module UnsupportedOptionalItem = {
    let raise = schema => {
      raise(UnsupportedOptionalItem(schema->S.name))
    }
  }

  module UnsupportedSchema = {
    let raise = schema => {
      raise(UnsupportedSchema(schema->S.name))
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
    | UnsupportedRootOptional => `Optional schema is not supported at root`
    | UnsupportedNestedOptional => `Optional schema is not supported inside the Option schema`
    | UnsupportedOptionalItem(schemaName) =>
      `Optional schema is not supported as ${schemaName} item`
    | UnsupportedSchema(schemaName) => `The ${schemaName} schema is not supported`
    | DefaultDestructingFailed({destructingErrorMessage}) =>
      `Couldn't destruct default value. Error: ${destructingErrorMessage}`
    }
    `[ReScript JSON Schema] Failed converting at ${pathText}. Reason: ${reason}`
  }
}

include JSONSchema7

@val
external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"

let schemaExtendMetadataId: S.Metadata.Id.t<t> = S.Metadata.Id.make(
  ~namespace="rescript-json-schema",
  ~name="schemaExtend",
)

let isOptionalSchema = schema =>
  switch schema->S.classify {
  | Option(_) => true
  | _ => false
  }

let rec fromRescriptSchema:
  type value. S.t<value> => t =
  schema => {
    let jsonSchema: Mutable.t = {}
    switch schema->S.classify {
    | S.String =>
      jsonSchema.type_ = Some(Arrayable.single(#string))
      schema
      ->S.String.refinements
      ->Js.Array2.forEach(refinement => {
        switch refinement {
        | {kind: Email} => jsonSchema.format = Some("email")
        | {kind: Url} => jsonSchema.format = Some("uri")
        | {kind: Uuid} => jsonSchema.format = Some("uuid")
        | {kind: Datetime} => jsonSchema.format = Some("date-time")
        | {kind: Cuid} => ()
        | {kind: Length({length})} => {
            jsonSchema.minLength = Some(length)
            jsonSchema.maxLength = Some(length)
          }
        | {kind: Max({length})} => jsonSchema.maxLength = Some(length)
        | {kind: Min({length})} => jsonSchema.minLength = Some(length)
        | {kind: Pattern({re})} => jsonSchema.pattern = Some(re->Js.String2.make)
        }
      })
    | S.Int => {
        jsonSchema.type_ = Some(Arrayable.single(#integer))
        schema
        ->S.Int.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Port} => ()
          | {kind: Max({value})} => jsonSchema.maximum = Some(value->Js.Int.toFloat)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value->Js.Int.toFloat)
          }
        })
      }
    | S.Bool => jsonSchema.type_ = Some(Arrayable.single(#boolean))
    | S.Float => {
        jsonSchema.type_ = Some(Arrayable.single(#number))
        schema
        ->S.Float.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({value})} => jsonSchema.maximum = Some(value)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value)
          }
        })
      }
    | S.Array(childSchema) => {
        if childSchema->isOptionalSchema {
          Error.UnsupportedOptionalItem.raise(schema)
        }
        jsonSchema.items = Some(
          Arrayable.single(Definition.schema(fromRescriptSchema(childSchema))),
        )
        jsonSchema.type_ = Some(Arrayable.single(#array))
        schema
        ->S.Array.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({length})} => jsonSchema.maxItems = Some(length)
          | {kind: Min({length})} => jsonSchema.minItems = Some(length)
          | {kind: Length({length})} => {
              jsonSchema.maxItems = Some(length)
              jsonSchema.minItems = Some(length)
            }
          }
        })
      }

    | S.Tuple({items}) => {
        let items = items->Js.Array2.mapi((item, idx) => {
          try {
            if item.schema->isOptionalSchema {
              Error.UnsupportedOptionalItem.raise(schema)
            } else {
              Definition.schema(fromRescriptSchema(item.schema))
            }
          } catch {
          | Error.Exception(error) =>
            raise(Error.Exception(error->Error.prependLocation(idx->Js.Int.toString)))
          }
        })
        let itemsNumber = items->Js.Array2.length

        jsonSchema.items = Some(Arrayable.array(items))
        jsonSchema.type_ = Some(Arrayable.single(#array))
        jsonSchema.minItems = Some(itemsNumber)
        jsonSchema.maxItems = Some(itemsNumber)
      }

    | S.Union(childSchemas) => {
        let items = childSchemas->Js.Array2.map(childSchema => {
          if childSchema->isOptionalSchema {
            Error.UnsupportedOptionalItem.raise(schema)
          } else {
            Definition.schema(fromRescriptSchema(childSchema))
          }
        })
        jsonSchema.anyOf = Some(items)
      }

    | S.Option(childSchema) => {
        if childSchema->isOptionalSchema {
          Error.raise(UnsupportedNestedOptional)
        }

        let childJsonSchema = fromRescriptSchema(childSchema)
        jsonSchema->Mutable.mixin(childJsonSchema)

        switch schema->S.Option.default {
        | Some(default) =>
          let defaultValue = switch default {
          | Value(v) => v
          | Callback(cb) => cb()
          }
          switch Some(defaultValue)
          ->(magic: option<unknown> => unknown)
          ->S.serializeWith(childSchema) {
          | Error(destructingError) =>
            Error.raise(
              DefaultDestructingFailed({
                destructingErrorMessage: destructingError->S.Error.message,
              }),
            )
          | Ok(destructedValue) => jsonSchema.default = Some(destructedValue)
          }
        | None => ()
        }
      }
    | S.Object({items, unknownKeys}) => {
        let properties = Js.Dict.empty()
        let required = []
        items->Js.Array2.forEach(item => {
          let fieldSchema = try {
            fromRescriptSchema(item.schema)
          } catch {
          | Error.Exception(error) =>
            raise(Error.Exception(error->Error.prependLocation(item.location)))
          }
          if item.schema->isOptionalSchema->not {
            required->Js.Array2.push(item.location)->ignore
          }
          properties->Js.Dict.set(item.location, Definition.schema(fieldSchema))
        })
        let additionalProperties = switch unknownKeys {
        | Strict => false
        | Strip => true
        }

        jsonSchema.type_ = Some(Arrayable.single(#object))
        jsonSchema.properties = Some(properties)
        jsonSchema.additionalProperties = Some(Definition.boolean(additionalProperties))
        switch required {
        | [] => ()
        | required => jsonSchema.required = Some(required)
        }
      }
    | S.JSON(_)
    | S.Unknown => ()
    | S.Null(childSchema) =>
      jsonSchema.anyOf = Some([
        Definition.schema(fromRescriptSchema(childSchema)),
        Definition.schema({
          type_: Arrayable.single(#null),
        }),
      ])

    | S.Never => jsonSchema.not = Some(Definition.schema({}))
    | S.Literal(Boolean({value})) => {
        jsonSchema.type_ = Some(Arrayable.single(#boolean))
        jsonSchema.const = Some(Js.Json.boolean(value))
      }
    | S.Literal(Number({value})) => {
        let isInt = mod_float(value, 1.) === 0.
        jsonSchema.type_ = Some(Arrayable.single(isInt ? #integer : #number))
        jsonSchema.const = Some(Js.Json.number(value))
      }
    | S.Literal(String({value})) => {
        jsonSchema.type_ = Some(Arrayable.single(#string))
        jsonSchema.const = Some(Js.Json.string(value))
      }
    | S.Literal(Null(_)) => jsonSchema.type_ = Some(Arrayable.single(#null))
    | S.Literal(Undefined(_))
    | S.Literal(BigInt(_))
    | S.Literal(Function(_))
    | S.Literal(Array(_))
    | S.Literal(Dict(_))
    | S.Literal(Symbol(_))
    | S.Literal(Object(_))
    | S.Literal(NaN(_)) =>
      Error.UnsupportedSchema.raise(schema)
    | S.Dict(childSchema) =>
      if childSchema->isOptionalSchema {
        Error.UnsupportedOptionalItem.raise(schema)
      }

      jsonSchema.type_ = Some(Arrayable.single(#object))
      jsonSchema.additionalProperties = Some(Definition.schema(fromRescriptSchema(childSchema)))
    }

    switch schema->S.description {
    | Some(m) => jsonSchema.description = Some(m)
    | None => ()
    }

    switch schema->S.deprecation {
    | Some(message) =>
      jsonSchema->Mutable.mixin({"deprecated": true, "description": message}->(magic: 'a => t))
    | None => ()
    }

    switch schema->S.Metadata.get(~id=schemaExtendMetadataId) {
    | Some(metadataRawSchema) => jsonSchema->Mutable.mixin(metadataRawSchema)
    | None => ()
    }

    jsonSchema->Mutable.toReadOnly
  }

let make = schema => {
  try {
    if schema->isOptionalSchema {
      Error.raise(UnsupportedRootOptional)
    } else {
      let jsonSchema = schema->fromRescriptSchema->Mutable.fromReadOnly
      jsonSchema.schema = Some("http://json-schema.org/draft-07/schema#")
      Ok(jsonSchema->Mutable.toReadOnly)
    }
  } catch {
  | Error.Exception(error) => Error(error->Error.toString)
  }
}

let extend = (schema, jsonSchema) => {
  schema->S.Metadata.set(
    ~id=schemaExtendMetadataId,
    switch schema->S.Metadata.get(~id=schemaExtendMetadataId) {
    | Some(existingSchemaExtend) => merge(existingSchemaExtend, jsonSchema)
    | None => jsonSchema
    },
  )
}

let castAnySchemaToJsonableS = (magic: S.t<'any> => S.t<Js.Json.t>)

@inline
let primitiveToSchema = primitive => {
  S.literal(primitive)->castAnySchemaToJsonableS
}

let toIntSchema = (jsonSchema: t) => {
  let schema = S.int
  // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
  // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
  //  r += `.multipleOf(${jsonSchema.multipleOf})`;
  // }
  let schema = switch jsonSchema {
  | {minimum} => schema->S.intMin(minimum->Belt.Float.toInt)
  | {exclusiveMinimum} => schema->S.intMin((exclusiveMinimum +. 1.)->Belt.Float.toInt)
  | _ => schema
  }
  let schema = switch jsonSchema {
  | {maximum} => schema->S.intMax(maximum->Belt.Float.toInt)
  | {exclusiveMinimum} => schema->S.intMax((exclusiveMinimum -. 1.)->Belt.Float.toInt)
  | _ => schema
  }
  schema->castAnySchemaToJsonableS
}

let definitionToDefaultValue = definition =>
  switch definition->Definition.classify {
  | Schema(s) => s.default
  | Boolean(_) => None
  }

let rec toRescriptSchema = (jsonSchema: t) => {
  let anySchema = S.json(~validate=false)

  let definitionToSchema = definition =>
    switch definition->Definition.classify {
    | Schema(s) => s->toRescriptSchema
    | Boolean(_) => anySchema
    }

  let schema = switch jsonSchema {
  | _ if (jsonSchema->(magic: t => {..}))["nullable"] =>
    S.null(
      jsonSchema->merge({"nullable": false}->(magic: {..} => t))->toRescriptSchema,
    )->castAnySchemaToJsonableS
  | {type_} if type_ === Arrayable.single(#object) =>
    let schema = switch jsonSchema.properties {
    | Some(properties) =>
      let schema = S.object(s =>
        properties
        ->Js.Dict.entries
        ->Js.Array2.map(((key, property)) => {
          let propertySchema = property->definitionToSchema
          let propertySchema = switch jsonSchema.required {
          | Some(r) if r->Js.Array2.includes(key) => propertySchema
          | _ =>
            switch property->definitionToDefaultValue {
            | Some(defaultValue) =>
              propertySchema->S.option->S.Option.getOr(defaultValue)->castAnySchemaToJsonableS
            | None => propertySchema->S.option->castAnySchemaToJsonableS
            }
          }
          (key, s.field(key, propertySchema))
        })
        ->Js.Dict.fromArray
      )
      let schema = switch jsonSchema {
      | {additionalProperties} if additionalProperties === Definition.boolean(false) =>
        schema->S.Object.strict
      | _ => schema
      }
      schema->castAnySchemaToJsonableS
    | None =>
      switch jsonSchema.additionalProperties {
      | Some(additionalProperties) =>
        switch additionalProperties->Definition.classify {
        | Boolean(true) => S.dict(anySchema)->castAnySchemaToJsonableS
        | Boolean(false) => S.object(_ => ())->S.Object.strict->castAnySchemaToJsonableS
        | Schema(s) => S.dict(s->toRescriptSchema)->castAnySchemaToJsonableS
        }
      | None => S.object(_ => ())->castAnySchemaToJsonableS
      }
    }

    // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
    schema
  | {type_} if type_ === Arrayable.single(#array) => {
      let schema = switch jsonSchema.items {
      | Some(items) =>
        switch items->Arrayable.classify {
        | Single(single) => S.array(single->definitionToSchema)
        | Array(array) =>
          S.tuple(s => array->Js.Array2.mapi((d, idx) => s.item(idx, d->definitionToSchema)))
        }
      | None => S.array(anySchema)
      }
      let schema = switch jsonSchema.minItems {
      | Some(min) => schema->S.arrayMinLength(min)
      | _ => schema
      }
      let schema = switch jsonSchema.maxItems {
      | Some(max) => schema->S.arrayMaxLength(max)
      | _ => schema
      }
      schema->castAnySchemaToJsonableS
    }
  | {anyOf: []} => anySchema
  | {anyOf: [d]} => d->definitionToSchema
  | {anyOf: definitions} => S.union(definitions->Js.Array2.map(definitionToSchema))
  | {allOf: []} => anySchema
  | {allOf: [d]} => d->definitionToSchema
  | {allOf: definitions} =>
    anySchema->S.refine(s => data => {
      definitions->Js.Array2.forEach(d => {
        switch data->S.parseWith(d->definitionToSchema) {
        | Ok(_) => ()
        | Error(_) => s.fail("Should pass for all schemas of the allOf property.")
        }
      })
    })
  | {oneOf: []} => anySchema
  | {oneOf: [d]} => d->definitionToSchema
  | {oneOf: definitions} =>
    anySchema->S.refine(s => data => {
      let hasOneValidRef = ref(false)
      definitions->Js.Array2.forEach(d => {
        switch data->S.parseWith(d->definitionToSchema) {
        | Ok(_) if hasOneValidRef.contents =>
          s.fail("Should pass single schema according to the oneOf property.")
        | Ok(_) => hasOneValidRef.contents = true
        | Error(_) => ()
        }
      })
      if hasOneValidRef.contents->not {
        s.fail("Should pass at least one schema according to the oneOf property.")
      }
    })
  | {not} =>
    anySchema->S.refine(s => data =>
      switch data->S.parseWith(not->definitionToSchema) {
      | Ok(_) => s.fail("Should NOT be valid against schema in the not property.")
      | Error(_) => ()
      })
  // needs to come before primitives
  | {enum: []} => anySchema
  | {enum: [p]} => p->primitiveToSchema
  | {enum: primitives} =>
    S.union(primitives->Js.Array2.map(primitiveToSchema))->castAnySchemaToJsonableS
  | {const} => const->primitiveToSchema
  | {type_} if type_->Arrayable.isArray =>
    let types = type_->(magic: Arrayable.t<'a> => array<'a>)
    S.union(
      types->Js.Array2.map(type_ => {
        jsonSchema->merge({type_: Arrayable.single(type_)})->toRescriptSchema
      }),
    )
  | {type_} if type_ === Arrayable.single(#string) =>
    let schema = S.string
    let schema = switch jsonSchema {
    | {pattern} => schema->S.pattern(Js.Re.fromString(pattern))
    | _ => schema
    }

    let schema = switch jsonSchema {
    | {minLength} => schema->S.stringMinLength(minLength)
    | _ => schema
    }
    let schema = switch jsonSchema {
    | {maxLength} => schema->S.stringMaxLength(maxLength)
    | _ => schema
    }
    switch jsonSchema {
    | {format: "email"} => schema->S.email->castAnySchemaToJsonableS
    | {format: "uri"} => schema->S.url->castAnySchemaToJsonableS
    | {format: "uuid"} => schema->S.uuid->castAnySchemaToJsonableS
    | {format: "date-time"} => schema->S.datetime->castAnySchemaToJsonableS
    | _ => schema->castAnySchemaToJsonableS
    }

  | {type_} if type_ === Arrayable.single(#integer) => jsonSchema->toIntSchema
  | {type_, format: "int64"} if type_ === Arrayable.single(#number) => jsonSchema->toIntSchema
  | {type_, multipleOf: 1.} if type_ === Arrayable.single(#number) => jsonSchema->toIntSchema
  | {type_} if type_ === Arrayable.single(#number) => {
      let schema = S.float
      let schema = switch jsonSchema {
      | {minimum} => schema->S.floatMin(minimum)
      | {exclusiveMinimum} => schema->S.floatMin(exclusiveMinimum +. 1.)
      | _ => schema
      }
      let schema = switch jsonSchema {
      | {maximum} => schema->S.floatMax(maximum)
      | {exclusiveMinimum} => schema->S.floatMax(exclusiveMinimum -. 1.)
      | _ => schema
      }
      schema->castAnySchemaToJsonableS
    }
  | {type_} if type_ === Arrayable.single(#boolean) => S.bool->castAnySchemaToJsonableS
  | {type_} if type_ === Arrayable.single(#null) =>
    S.literal(%raw(`null`))->castAnySchemaToJsonableS
  | {if_, then, else_} => {
      let ifSchema = if_->definitionToSchema
      let thenSchema = then->definitionToSchema
      let elseSchema = else_->definitionToSchema
      anySchema->S.refine(_ => data => {
        let result = switch data->S.parseWith(ifSchema) {
        | Ok(_) => data->S.parseWith(thenSchema)
        | Error(_) => data->S.parseWith(elseSchema)
        }
        switch result {
        | Ok(_) => ()
        | Error(e) => S.Error.raise(e)
        }
      })
    }
  | _ => anySchema
  }

  let schema = switch jsonSchema {
  | {description} => schema->S.describe(description)
  | _ => schema
  }

  let schema = switch jsonSchema {
  | {description} => schema->S.describe(description)
  | _ => schema
  }

  schema
}
