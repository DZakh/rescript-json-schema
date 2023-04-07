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

let castAnyStructToUnknownStruct = (Obj.magic: S.t<'any> => S.t<unknown>)

let rec primitiveToStruct = primitive => {
  switch primitive->Js.Json.classify {
  | JSONTrue => S.literal(Bool(true))->castAnyStructToUnknownStruct
  | JSONFalse => S.literal(Bool(false))->castAnyStructToUnknownStruct
  | JSONNull => S.literal(EmptyNull)->castAnyStructToUnknownStruct
  | JSONString(s) => S.literal(String(s))->castAnyStructToUnknownStruct
  | JSONNumber(n) if n < 2147483648. && n > -2147483649. && mod_float(n, 1.) === 0. =>
    S.literal(Int(n->(Obj.magic: float => int)))->castAnyStructToUnknownStruct
  | JSONNumber(n) => S.literal(Float(n))->castAnyStructToUnknownStruct
  | JSONObject(d) =>
    S.object(o =>
      d
      ->Js.Dict.entries
      ->Js.Array2.map(((key, value)) => {
        (key, o->S.field(key, value->primitiveToStruct))
      })
      ->Js.Dict.fromArray
    )->castAnyStructToUnknownStruct
  | JSONArray(a) =>
    S.Tuple.factory(a->Js.Array2.map(primitiveToStruct))->castAnyStructToUnknownStruct
  }
}

let toIntStruct = (schema: t) => {
  let struct = S.int()
  // TODO: Support schema.multipleOf when it's in rescript-struct
  // if (typeof schema.multipleOf === "number" && schema.multipleOf !== 1) {
  //  r += `.multipleOf(${schema.multipleOf})`;
  // }
  let struct = switch schema {
  | {minimum} => struct->S.Int.min(minimum->Belt.Float.toInt)
  | {exclusiveMinimum} => struct->S.Int.min((exclusiveMinimum +. 1.)->Belt.Float.toInt)
  | _ => struct
  }
  let struct = switch schema {
  | {maximum} => struct->S.Int.max(maximum->Belt.Float.toInt)
  | {exclusiveMinimum} => struct->S.Int.max((exclusiveMinimum -. 1.)->Belt.Float.toInt)
  | _ => struct
  }
  struct->castAnyStructToUnknownStruct
}

let rec toStruct = (schema: t) => {
  let definitionToStruct = definition =>
    switch definition->Definition.classify {
    | Schema(s) => s->toStruct
    | Boolean(_) => S.unknown()
    }

  let struct = switch schema {
  | _ if (schema->(Obj.magic: t => {..}))["nullable"] =>
    S.null(
      schema->Schema.merge({"nullable": false}->(Obj.magic: {..} => t))->toStruct,
    )->castAnyStructToUnknownStruct
  | {type_} if type_ === Arrayable.single(#object) =>
    let struct = switch schema.properties {
    | Some(properties) =>
      let struct = S.object(o =>
        properties
        ->Js.Dict.entries
        ->Js.Array2.map(((key, property)) => {
          let propertyStruct = property->definitionToStruct
          let propertyStruct = switch schema.required {
          | Some(r) if r->Js.Array2.includes(key) => propertyStruct
          | _ => propertyStruct->S.option->castAnyStructToUnknownStruct
          }
          (key, o->S.field(key, propertyStruct))
        })
        ->Js.Dict.fromArray
      )
      let struct = switch schema {
      | {additionalProperties} if additionalProperties === Definition.boolean(false) =>
        struct->S.Object.strict
      | _ => struct
      }
      struct->castAnyStructToUnknownStruct
    | None =>
      switch schema.additionalProperties {
      | Some(additionalProperties) =>
        switch additionalProperties->Definition.classify {
        | Boolean(true) => S.dict(S.unknown())->castAnyStructToUnknownStruct
        | Boolean(false) => S.object(_ => ())->S.Object.strict->castAnyStructToUnknownStruct
        | Schema(s) => S.dict(s->toStruct)->castAnyStructToUnknownStruct
        }
      | None => S.object(_ => ())->castAnyStructToUnknownStruct
      }
    }

    // TODO: schema.anyOf and schema.oneOf support
    struct
  | {type_} if type_ === Arrayable.single(#array) => {
      let struct = switch schema.items {
      | Some(items) =>
        switch items->Arrayable.classify {
        | Single(single) => S.array(single->definitionToStruct)
        | Array(array) => S.Tuple.factory(array->Js.Array2.map(definitionToStruct))
        }
      | None => S.array(S.unknown())
      }
      let struct = switch schema.minItems {
      | Some(min) => struct->S.Array.min(min)
      | _ => struct
      }
      let struct = switch schema.maxItems {
      | Some(max) => struct->S.Array.max(max)
      | _ => struct
      }
      struct->castAnyStructToUnknownStruct
    }
  | {anyOf: []} => S.unknown()
  | {anyOf: [d]} => d->definitionToStruct
  | {anyOf: definitions} => S.union(definitions->Js.Array2.map(definitionToStruct))
  | {allOf: []} => S.unknown()
  | {allOf: [d]} => d->definitionToStruct
  | {allOf: definitions} => {
      let refiner = data => {
        definitions->Js.Array2.forEach(d => {
          switch data->S.parseAnyWith(d->definitionToStruct) {
          | Ok(_) => ()
          | Error(_) => S.fail("Should pass for all schemas of the allOf property.")
          }
        })
      }
      S.unknown()->S.refine(~parser=refiner, ~serializer=refiner, ())
    }
  | {oneOf: []} => S.unknown()
  | {oneOf: [d]} => d->definitionToStruct
  | {oneOf: definitions} =>
    let refiner = data => {
      let hasOneValidRef = ref(false)
      definitions->Js.Array2.forEach(d => {
        switch data->S.parseAnyWith(d->definitionToStruct) {
        | Ok(_) if hasOneValidRef.contents =>
          S.fail("Should pass single schema according to the oneOf property.")
        | Ok(_) => hasOneValidRef.contents = true
        | Error(_) => ()
        }
      })
      if hasOneValidRef.contents->not {
        S.fail("Should pass at least one schema according to the oneOf property.")
      }
    }
    S.unknown()->S.refine(~parser=refiner, ~serializer=refiner, ())
  | {not} => {
      let refiner = data =>
        switch data->S.parseAnyWith(not->definitionToStruct) {
        | Ok(_) => S.fail("Should NOT be valid against schema in the not property.")
        | Error(_) => ()
        }
      S.unknown()->S.refine(~parser=refiner, ~serializer=refiner, ())
    }
  // needs to come before primitives
  | {enum: []} => S.unknown()
  | {enum: [p]} => p->primitiveToStruct
  | {enum: primitives} =>
    S.union(primitives->Js.Array2.map(primitiveToStruct))->castAnyStructToUnknownStruct
  | {const} => const->primitiveToStruct
  | {type_} if type_->Arrayable.isArray =>
    let types = type_->(Obj.magic: Arrayable.t<'a> => array<'a>)
    S.union(
      types->Js.Array2.map(type_ => {
        schema->Schema.merge({type_: Arrayable.single(type_)})->toStruct
      }),
    )
  | {type_} if type_ === Arrayable.single(#string) =>
    let struct = S.string()
    let struct = switch schema {
    | {pattern} => struct->S.String.pattern(Js.Re.fromString(pattern))
    | _ => struct
    }
    let struct = switch schema {
    | {format: "email"} => struct->S.String.email()
    | {format: "uri"} => struct->S.String.url()
    | {format: "uuid"} => struct->S.String.uuid()
    | _ => struct
    }
    let struct = switch schema {
    | {minLength} => struct->S.String.min(minLength)
    | _ => struct
    }
    let struct = switch schema {
    | {maxLength} => struct->S.String.min(maxLength)
    | _ => struct
    }
    struct->castAnyStructToUnknownStruct

  | {type_} if type_ === Arrayable.single(#integer) => schema->toIntStruct
  | {type_, format: "int64"} if type_ === Arrayable.single(#number) => schema->toIntStruct
  | {type_, multipleOf: 1.} if type_ === Arrayable.single(#number) => schema->toIntStruct
  | {type_} if type_ === Arrayable.single(#number) => {
      let struct = S.float()
      let struct = switch schema {
      | {minimum} => struct->S.Float.min(minimum)
      | {exclusiveMinimum} => struct->S.Float.min(exclusiveMinimum +. 1.)
      | _ => struct
      }
      let struct = switch schema {
      | {maximum} => struct->S.Float.max(maximum)
      | {exclusiveMinimum} => struct->S.Float.max(exclusiveMinimum -. 1.)
      | _ => struct
      }
      struct->castAnyStructToUnknownStruct
    }
  | {type_} if type_ === Arrayable.single(#boolean) => S.bool()->castAnyStructToUnknownStruct
  | {type_} if type_ === Arrayable.single(#null) =>
    S.literal(EmptyNull)->castAnyStructToUnknownStruct
  | {if_, then, else_} => {
      let ifStruct = if_->definitionToStruct
      let thenStruct = then->definitionToStruct
      let elseStruct = else_->definitionToStruct
      let refiner = data => {
        let result = switch data->S.parseAnyWith(ifStruct) {
        | Ok(_) => data->S.parseAnyWith(thenStruct)
        | Error(_) => data->S.parseAnyWith(elseStruct)
        }
        switch result {
        | Ok(_) => ()
        | Error(e) => S.advancedFail(e)
        }
      }
      S.unknown()->S.refine(~parser=refiner, ~serializer=refiner, ())
    }
  | _ => S.unknown()
  }

  let struct = switch schema {
  | {description} => struct->S.describe(description)
  | _ => struct
  }

  let struct = switch schema {
  | {description} => struct->S.describe(description)
  | _ => struct
  }

  let struct = switch schema {
  | {default} =>
    struct
    ->(Obj.magic: S.t<unknown> => S.t<option<unknown>>)
    ->S.default(() => default->(Obj.magic: Js.Json.t => unknown))
  | _ => struct
  }

  struct
}

let validate = schema => {
  let struct = schema->toStruct
  data =>
    switch data->S.parseWith(struct) {
    | Ok(_) => Ok()
    | Error(_) as e => e
    }
}
