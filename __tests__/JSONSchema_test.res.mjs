// Generated by ReScript, PLEASE EDIT WITH CARE

import Ava from "ava";
import * as JSONSchema from "../src/JSONSchema.res.mjs";
import * as S$RescriptSchema from "rescript-schema/src/S.res.mjs";

Ava("Schema of bool schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.bool), {
              TAG: "Ok",
              _0: {"$schema": "http://json-schema.org/draft-07/schema#", "type": "boolean"}
            });
      }));

Ava("Schema of string schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.string), {
              TAG: "Ok",
              _0: {"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}
            });
      }));

Ava("Schema of string schema with Email refinement", (function (t) {
        var schema = S$RescriptSchema.email(S$RescriptSchema.string, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "email"
      }
            });
      }));

Ava("Schema of string schema with Url refinement", (function (t) {
        var schema = S$RescriptSchema.url(S$RescriptSchema.string, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uri"
      }
            });
      }));

Ava("Schema of string schema with Datetime refinement", (function (t) {
        var schema = S$RescriptSchema.datetime(S$RescriptSchema.string, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }
            });
      }));

Ava("Schema of string schema uses the last refinement for format", (function (t) {
        var schema = S$RescriptSchema.datetime(S$RescriptSchema.email(S$RescriptSchema.string, undefined), undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "date-time"
      }
            });
      }));

Ava("Schema of string schema with Cuid refinement", (function (t) {
        var schema = S$RescriptSchema.cuid(S$RescriptSchema.string, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string"
      }
            });
      }));

Ava("Schema of string schema with Uuid refinement", (function (t) {
        var schema = S$RescriptSchema.uuid(S$RescriptSchema.string, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "format": "uuid"
      }
            });
      }));

Ava("Schema of string schema with Pattern refinement", (function (t) {
        var schema = S$RescriptSchema.pattern(S$RescriptSchema.string, /abc/g, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "pattern": "/abc/g"
      }
            });
      }));

Ava("Schema of string schema with Min refinement", (function (t) {
        var schema = S$RescriptSchema.stringMinLength(S$RescriptSchema.string, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1
      }
            });
      }));

Ava("Schema of string schema with Max refinement", (function (t) {
        var schema = S$RescriptSchema.stringMaxLength(S$RescriptSchema.string, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "maxLength": 1
      }
            });
      }));

Ava("Schema of string schema with Length refinement", (function (t) {
        var schema = S$RescriptSchema.stringLength(S$RescriptSchema.string, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 1
      }
            });
      }));

Ava("Schema of string schema with both Min and Max refinements", (function (t) {
        var schema = S$RescriptSchema.stringMaxLength(S$RescriptSchema.stringMinLength(S$RescriptSchema.string, 1, undefined), 4, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "minLength": 1,
        "maxLength": 4
      }
            });
      }));

Ava("Schema of int schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.$$int), {
              TAG: "Ok",
              _0: {"$schema": "http://json-schema.org/draft-07/schema#", "type": "integer"}
            });
      }));

Ava("Schema of int schema with Min refinement", (function (t) {
        var schema = S$RescriptSchema.intMin(S$RescriptSchema.$$int, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "minimum": 1
      }
            });
      }));

Ava("Schema of int schema with Max refinement", (function (t) {
        var schema = S$RescriptSchema.intMax(S$RescriptSchema.$$int, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "maximum": 1
      }
            });
      }));

Ava("Schema of int schema with Port refinement", (function (t) {
        var schema = S$RescriptSchema.port(S$RescriptSchema.$$int, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer"
      }
            });
      }));

Ava("Schema of float schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.$$float), {
              TAG: "Ok",
              _0: {"$schema": "http://json-schema.org/draft-07/schema#", "type": "number"}
            });
      }));

Ava("Schema of float schema with Min refinement", (function (t) {
        var schema = S$RescriptSchema.floatMin(S$RescriptSchema.$$float, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "minimum": 1
      }
            });
      }));

Ava("Schema of float schema with Max refinement", (function (t) {
        var schema = S$RescriptSchema.floatMax(S$RescriptSchema.$$float, 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "maximum": 1
      }
            });
      }));

Ava("Schema of Null schema", (function (t) {
        var schema = S$RescriptSchema.$$null(S$RescriptSchema.$$float);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [{"type": "number"}, {"type": "null"}]
      }
            });
      }));

Ava("Schema of Never schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.never), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "not": {}
      }
            });
      }));

Ava("Schema of Bool Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal(false);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "const": false
      }
            });
      }));

Ava("Schema of String Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal("Hello World!");
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "const": "Hello World!"
      }
            });
      }));

Ava("Schema of Object Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal({
              received: true
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": false,
        "properties": {
          "received": {
            "type": "boolean",
            "const": true
          }
        },
        "required": ["received"]
      }
            });
      }));

Ava("Schema of Int Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal(123);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "integer",
        "const": 123
      }
            });
      }));

Ava("Schema of Float Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal(-123.456);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "number",
        "const": -123.456
      }
            });
      }));

Ava("Schema of EmptyNull Literal schema", (function (t) {
        var schema = S$RescriptSchema.literal(null);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "null",
      }
            });
      }));

Ava("Schema of EmptyOption Literal schema isn't supported", (function (t) {
        var schema = S$RescriptSchema.literal(undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: The undefined schema is not supported"
            });
      }));

Ava("Schema of NaN Literal schema isn't supported", (function (t) {
        var schema = S$RescriptSchema.literal(NaN);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: The NaN schema is not supported"
            });
      }));

Ava("Schema of tuple schema", (function (t) {
        var schema = S$RescriptSchema.tuple2(S$RescriptSchema.string, S$RescriptSchema.bool);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "minItems": 2,
        "maxItems": 2,
        "items": [{"type": "string"}, {"type": "boolean"}],
      }
            });
      }));

Ava("Schema of enum schema", (function (t) {
        var schema = S$RescriptSchema.$$enum([
              "Yes",
              "No"
            ]);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "enum": ["Yes", "No"]
      }
            });
      }));

Ava("Schema of union schema", (function (t) {
        var schema = S$RescriptSchema.union([
              S$RescriptSchema.literal("Yes"),
              S$RescriptSchema.string
            ]);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [
          {
            const: 'Yes',
            type: 'string'
          },
          {
            type: 'string'
          }
        ]
      }
            });
      }));

Ava("Schema of strings array schema", (function (t) {
        var schema = S$RescriptSchema.array(S$RescriptSchema.string);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
      }
            });
      }));

Ava("Schema of array schema with Min refinement", (function (t) {
        var schema = S$RescriptSchema.arrayMinLength(S$RescriptSchema.array(S$RescriptSchema.string), 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1
      }
            });
      }));

Ava("Schema of array schema with Max refinement", (function (t) {
        var schema = S$RescriptSchema.arrayMaxLength(S$RescriptSchema.array(S$RescriptSchema.string), 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "maxItems": 1
      }
            });
      }));

Ava("Schema of array schema with Length refinement", (function (t) {
        var schema = S$RescriptSchema.arrayLength(S$RescriptSchema.array(S$RescriptSchema.string), 1, undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "array",
        "items": {"type": "string"},
        "minItems": 1,
        "maxItems": 1
      }
            });
      }));

Ava("Schema of strings dict schema", (function (t) {
        var schema = S$RescriptSchema.dict(S$RescriptSchema.string);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": {"type": "string"},
      }
            });
      }));

Ava("Schema of object schema with one string field", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("field", S$RescriptSchema.string);
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with one string discriminant", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              s.f("field", S$RescriptSchema.string);
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with Strip unknownKeys strategy allows additionalProperties", (function (t) {
        var schema = S$RescriptSchema.strip(S$RescriptSchema.object(function (s) {
                  return s.f("field", S$RescriptSchema.string);
                }));
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with Strict unknownKeys strategy disallows additionalProperties", (function (t) {
        var schema = S$RescriptSchema.strict(S$RescriptSchema.object(function (s) {
                  return s.f("field", S$RescriptSchema.string);
                }));
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"field": {"type": "string"}},
        "required": ["field"],
        "additionalProperties": false,
      }
            });
      }));

Ava("Schema of object schema with one optional string field", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("optionalField", S$RescriptSchema.option(S$RescriptSchema.string));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {"optionalField": {"type": "string"}},
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with one deprecated string field", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("field", S$RescriptSchema.deprecate(S$RescriptSchema.string, "Use another field"));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "required": [ "field" ],
        "additionalProperties": true,
      }
            });
      }));

Ava("Deprecated message overrides previous description", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("field", S$RescriptSchema.deprecate(S$RescriptSchema.describe(S$RescriptSchema.string, "Previous description"), "Use another field"));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {"type": "string", "deprecated": true, "description": "Use another field"},
        },
        "required": [ 'field' ],
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with nested object", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("objectWithOneStringField", S$RescriptSchema.object(function (s) {
                              return s.f("Field", S$RescriptSchema.string);
                            }));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "objectWithOneStringField": {
            "type": "object",
            "properties": {"Field": {"type": "string"}},
            "required": ["Field"],
            "additionalProperties": true,
          },
        },
        "required": ["objectWithOneStringField"],
        "additionalProperties": true,
      }
            });
      }));

Ava("Schema of object schema with one optional and one required string field", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return [
                      s.f("field", S$RescriptSchema.string),
                      s.f("optionalField", S$RescriptSchema.option(S$RescriptSchema.string))
                    ];
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "field": {
            "type": "string",
          },
          "optionalField": {"type": "string"},
        },
        "required": ["field"],
        "additionalProperties": true,
      }
            });
      }));

Ava("Make JSONSchema throws error with optional root type", (function (t) {
        var schema = S$RescriptSchema.option(S$RescriptSchema.string);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported at root"
            });
      }));

Ava("Make JSONSchema throws error with object field wrapped in option multiple times", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("optionalOptionalField", S$RescriptSchema.option(S$RescriptSchema.option(S$RescriptSchema.string)));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at [\"optionalOptionalField\"]. Reason: Optional schema is not supported inside the Option schema"
            });
      }));

Ava("Primitive schema schema with description", (function (t) {
        var schema = S$RescriptSchema.describe(S$RescriptSchema.bool, "A primitive schema");
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "A primitive schema",
      }
            });
      }));

Ava("Transformed schema schema with default fails when destruction failed", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("field", S$RescriptSchema.$$Option.getOr(S$RescriptSchema.option(S$RescriptSchema.transform(S$RescriptSchema.bool, (function (param) {
                                        return {
                                                p: (function (bool) {
                                                    if (bool) {
                                                      return "true";
                                                    } else {
                                                      return "";
                                                    }
                                                  })
                                              };
                                      }))), "true"));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at [\"field\"]. Reason: Couldn't destruct default value. Error: Failed reverse converting to JSON at root. Reason: The S.transform serializer is missing"
            });
      }));

Ava("Transformed schema schema uses default with correct type", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("field", S$RescriptSchema.$$Option.getOrWith(S$RescriptSchema.option(S$RescriptSchema.transform(S$RescriptSchema.bool, (function (param) {
                                        return {
                                                p: (function (bool) {
                                                    if (bool) {
                                                      return "true";
                                                    } else {
                                                      return "";
                                                    }
                                                  }),
                                                s: (function (string) {
                                                    if (string === "true") {
                                                      return true;
                                                    } else {
                                                      return false;
                                                    }
                                                  })
                                              };
                                      }))), (function () {
                                return "true";
                              })));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "additionalProperties": true,
        "properties": {"field": {"default": true, "type": "boolean"}},
        "type": "object",
      }
            });
      }));

Ava("Primitive schema schema with additional raw schema", (function (t) {
        var schema = JSONSchema.extend(S$RescriptSchema.bool, {
              description: "foo"
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "description": "foo",
      }
            });
      }));

Ava("Primitive schema with an example", (function (t) {
        var schema = JSONSchema.example(S$RescriptSchema.bool, true);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "examples": [true],
      }
            });
      }));

Ava("Transformed schema with an example", (function (t) {
        var schema = JSONSchema.example(S$RescriptSchema.$$null(S$RescriptSchema.bool), undefined);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "anyOf": [{"type": "boolean"}, {"type": "null"}],
        "examples": [null],
      }
            });
      }));

Ava("Multiple examples", (function (t) {
        var schema = JSONSchema.example(JSONSchema.example(S$RescriptSchema.string, "Hi"), "It's me");
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "string",
        "examples": ["Hi", "It's me"],
      }
            });
      }));

Ava("Multiple additional raw schemas are merged together", (function (t) {
        var schema = JSONSchema.extend(JSONSchema.extend(S$RescriptSchema.bool, {
                  nullable: true
                }), {
              deprecated: true
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "boolean",
        "deprecated": true,
        "nullable": true,
      }
            });
      }));

Ava("Additional raw schema works with optional fields", (function (t) {
        var schema = S$RescriptSchema.object(function (s) {
              return s.f("optionalField", JSONSchema.extend(S$RescriptSchema.option(S$RescriptSchema.string), {
                              nullable: true
                            }));
            });
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "properties": {
          "optionalField": {"nullable": true, "type": "string"},
        },
        "additionalProperties": true,
      }
            });
      }));

Ava("Unknown schema doesn't affect final schema", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.unknown), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
      }
            });
      }));

Ava("JSON schema doesn't affect final schema", (function (t) {
        var schema = S$RescriptSchema.json(false);
        t.deepEqual(JSONSchema.make(schema), {
              TAG: "Ok",
              _0: {
        "$schema": "http://json-schema.org/draft-07/schema#",
      }
            });
      }));

Ava("Fails to create schema for schemas with optional items", (function (t) {
        t.deepEqual(JSONSchema.make(S$RescriptSchema.dict(S$RescriptSchema.option(S$RescriptSchema.string))), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as dict<string | undefined> item"
            });
        t.deepEqual(JSONSchema.make(S$RescriptSchema.array(S$RescriptSchema.option(S$RescriptSchema.string))), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as array<string | undefined> item"
            });
        t.deepEqual(JSONSchema.make(S$RescriptSchema.union([
                      S$RescriptSchema.option(S$RescriptSchema.string),
                      S$RescriptSchema.$$null(S$RescriptSchema.string)
                    ])), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as string | undefined | string | null item"
            });
        t.deepEqual(JSONSchema.make(S$RescriptSchema.tuple1(S$RescriptSchema.option(S$RescriptSchema.string))), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at [\"0\"]. Reason: Optional schema is not supported as [string | undefined] item"
            });
        t.deepEqual(JSONSchema.make(S$RescriptSchema.tuple1(S$RescriptSchema.array(S$RescriptSchema.option(S$RescriptSchema.string)))), {
              TAG: "Error",
              _0: "[ReScript JSON Schema] Failed converting at [\"0\"]. Reason: Optional schema is not supported as array<string | undefined> item"
            });
      }));

Ava("Example", (function (t) {
        var filmSchema = S$RescriptSchema.object(function (s) {
              return {
                      id: s.f("Id", S$RescriptSchema.$$float),
                      title: s.f("Title", S$RescriptSchema.string),
                      tags: s.fieldOr("Tags", S$RescriptSchema.array(S$RescriptSchema.string), []),
                      rating: s.f("Rating", S$RescriptSchema.union([
                                S$RescriptSchema.literal("G"),
                                S$RescriptSchema.literal("PG"),
                                S$RescriptSchema.literal("PG13"),
                                S$RescriptSchema.literal("R")
                              ])),
                      deprecatedAgeRestriction: s.f("Age", S$RescriptSchema.deprecate(S$RescriptSchema.option(S$RescriptSchema.$$int), "Use rating instead"))
                    };
            });
        t.deepEqual(JSONSchema.make(filmSchema), {
              TAG: "Ok",
              _0: {
          $schema: "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            Id: { type: "number" },
            Title: { type: "string" },
            Tags: { items: { type: "string" }, type: "array", default: [] },
            Rating: {
              enum: ["G", "PG", "PG13", "R"],
            },
            Age: {
              type: "integer",
              deprecated: true,
              description: "Use rating instead",
            },
          },
          additionalProperties: true,
          required: ["Id", "Title", "Rating"],
        }
            });
      }));

var Example = {};

export {
  Example ,
}
/*  Not a pure module */
