// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var JSONSchema7 = require("./JSONSchema7.bs.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");
var S$RescriptStruct = require("rescript-struct/src/S.bs.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

var Exception = /* @__PURE__ */Caml_exceptions.create("JSONSchema.Error.Exception");

function raise(code) {
  throw {
        RE_EXN_ID: Exception,
        _1: {
          code: code,
          path: []
        },
        Error: new Error()
      };
}

function pathToText(path) {
  if (path.length !== 0) {
    return path.map(function (pathItem) {
                  return "[\"" + pathItem + "\"]";
                }).join("");
  } else {
    return "root";
  }
}

function prependLocation(error, $$location) {
  error.path = [$$location].concat(error.path);
  return error;
}

function toString(error) {
  var pathText = pathToText(error.path);
  var structName = error.code;
  var reason;
  if (typeof structName === "number") {
    reason = structName === /* UnsupportedNestedOptional */0 ? "Optional struct is not supported inside the Option struct" : "Optional struct is not supported at root";
  } else {
    switch (structName.TAG | 0) {
      case /* UnsupportedOptionalItem */0 :
          reason = "Optional struct is not supported as " + structName._0 + " item";
          break;
      case /* UnsupportedStruct */1 :
          reason = "The " + structName._0 + " struct is not supported";
          break;
      case /* DefaultDestructingFailed */2 :
          reason = "Couldn't destruct default value. Error: " + structName.destructingErrorMessage + "";
          break;
      
    }
  }
  return "[ReScript JSON Schema] Failed converting at " + pathText + ". Reason: " + reason + "";
}

function description(value) {
  return {
          description: value
        };
}

function $$default(value) {
  return {
          default: Caml_option.some(value)
        };
}

function schemaDialect(param) {
  return {
          $schema: "http://json-schema.org/draft-07/schema#"
        };
}

function string(param) {
  return {
          type: "string"
        };
}

function integer(param) {
  return {
          type: "integer"
        };
}

function number(param) {
  return {
          type: "number"
        };
}

function $$boolean(param) {
  return {
          type: "boolean"
        };
}

function $$null(childSchema) {
  return {
          anyOf: [
            childSchema,
            {
              type: "null"
            }
          ]
        };
}

function never(param) {
  return {
          not: {}
        };
}

function array(childSchema) {
  return {
          type: "array",
          items: Caml_option.some(childSchema)
        };
}

function tuple(items) {
  var itemsNumber = items.length;
  return {
          type: "array",
          items: Caml_option.some(items),
          maxItems: itemsNumber,
          minItems: itemsNumber
        };
}

function union(items) {
  return {
          anyOf: items
        };
}

function dict(childSchema) {
  return {
          type: "object",
          additionalProperties: Caml_option.some(childSchema)
        };
}

function record(properties, additionalProperties, required) {
  var schema_type = "object";
  var schema_properties = Caml_option.some(properties);
  var schema_additionalProperties = Caml_option.some(additionalProperties);
  var schema = {
    type: schema_type,
    properties: schema_properties,
    additionalProperties: schema_additionalProperties
  };
  if (required.length !== 0) {
    schema.required = required;
  }
  return schema;
}

function deprecatedWithMessage(message) {
  return {
          deprecated: true,
          description: message
        };
}

function string$1(value) {
  return {
          type: "string",
          const: Caml_option.some(value)
        };
}

function integer$1(value) {
  return {
          type: "integer",
          const: Caml_option.some(value)
        };
}

function number$1(value) {
  return {
          type: "number",
          const: Caml_option.some(value)
        };
}

function $$boolean$1(value) {
  return {
          type: "boolean",
          const: Caml_option.some(value)
        };
}

function $$null$1(param) {
  return {
          type: "null"
        };
}

var schemaExtendMetadataId = Curry._2(S$RescriptStruct.Metadata.Id.make, "rescript-json-schema", "schemaExtend");

function isOptionalStruct(struct) {
  var match = S$RescriptStruct.classify(struct);
  if (typeof match === "number" || match.TAG !== /* Option */1) {
    return false;
  } else {
    return true;
  }
}

function makeStructSchema(struct) {
  var childStruct = S$RescriptStruct.classify(struct);
  var schema;
  if (typeof childStruct === "number") {
    switch (childStruct) {
      case /* Never */0 :
          schema = never(undefined);
          break;
      case /* Unknown */1 :
          schema = {};
          break;
      case /* String */2 :
          schema = string(undefined);
          break;
      case /* Int */3 :
          schema = integer(undefined);
          break;
      case /* Float */4 :
          schema = number(undefined);
          break;
      case /* Bool */5 :
          schema = $$boolean(undefined);
          break;
      
    }
  } else {
    switch (childStruct.TAG | 0) {
      case /* Literal */0 :
          var value = childStruct._0;
          if (typeof value === "number") {
            switch (value) {
              case /* EmptyNull */0 :
                  schema = $$null$1(undefined);
                  break;
              case /* EmptyOption */1 :
              case /* NaN */2 :
                  schema = raise({
                        TAG: /* UnsupportedStruct */1,
                        _0: S$RescriptStruct.name(struct)
                      });
                  break;
              
            }
          } else {
            switch (value.TAG | 0) {
              case /* String */0 :
                  schema = string$1(value._0);
                  break;
              case /* Int */1 :
                  schema = integer$1(value._0);
                  break;
              case /* Float */2 :
                  schema = number$1(value._0);
                  break;
              case /* Bool */3 :
                  schema = $$boolean$1(value._0);
                  break;
              
            }
          }
          break;
      case /* Option */1 :
          var childStruct$1 = childStruct._0;
          schema = isOptionalStruct(childStruct$1) ? raise(/* UnsupportedNestedOptional */0) : makeStructSchema(childStruct$1);
          break;
      case /* Null */2 :
          schema = $$null(makeStructSchema(childStruct._0));
          break;
      case /* Array */3 :
          var childStruct$2 = childStruct._0;
          schema = isOptionalStruct(childStruct$2) ? raise({
                  TAG: /* UnsupportedOptionalItem */0,
                  _0: S$RescriptStruct.name(struct)
                }) : array(makeStructSchema(childStruct$2));
          break;
      case /* Object */4 :
          var fields = childStruct.fields;
          var properties = {};
          var required = [];
          childStruct.fieldNames.forEach(function (fieldName) {
                var fieldStruct = fields[fieldName];
                var fieldSchema;
                try {
                  fieldSchema = makeStructSchema(fieldStruct);
                }
                catch (raw_error){
                  var error = Caml_js_exceptions.internalToOCamlException(raw_error);
                  if (error.RE_EXN_ID === Exception) {
                    throw {
                          RE_EXN_ID: Exception,
                          _1: prependLocation(error._1, fieldName),
                          Error: new Error()
                        };
                  }
                  throw error;
                }
                if (!isOptionalStruct(fieldStruct)) {
                  required.push(fieldName);
                }
                properties[fieldName] = fieldSchema;
              });
          var match = Curry._1(S$RescriptStruct.$$Object.UnknownKeys.classify, struct);
          schema = record(properties, match ? true : false, required);
          break;
      case /* Tuple */5 :
          schema = tuple(childStruct._0.map(function (childStruct, idx) {
                    try {
                      if (isOptionalStruct(childStruct)) {
                        return raise({
                                    TAG: /* UnsupportedOptionalItem */0,
                                    _0: S$RescriptStruct.name(struct)
                                  });
                      } else {
                        return makeStructSchema(childStruct);
                      }
                    }
                    catch (raw_error){
                      var error = Caml_js_exceptions.internalToOCamlException(raw_error);
                      if (error.RE_EXN_ID === Exception) {
                        throw {
                              RE_EXN_ID: Exception,
                              _1: prependLocation(error._1, idx.toString()),
                              Error: new Error()
                            };
                      }
                      throw error;
                    }
                  }));
          break;
      case /* Union */6 :
          schema = union(childStruct._0.map(function (childStruct) {
                    if (isOptionalStruct(childStruct)) {
                      return raise({
                                  TAG: /* UnsupportedOptionalItem */0,
                                  _0: S$RescriptStruct.name(struct)
                                });
                    } else {
                      return makeStructSchema(childStruct);
                    }
                  }));
          break;
      case /* Dict */7 :
          var childStruct$3 = childStruct._0;
          schema = isOptionalStruct(childStruct$3) ? raise({
                  TAG: /* UnsupportedOptionalItem */0,
                  _0: S$RescriptStruct.name(struct)
                }) : dict(makeStructSchema(childStruct$3));
          break;
      
    }
  }
  var match$1 = S$RescriptStruct.Deprecated.classify(struct);
  if (match$1 !== undefined) {
    if (match$1) {
      Object.assign(schema, deprecatedWithMessage(match$1._0));
    } else {
      Object.assign(schema, {
            deprecated: true
          });
    }
  }
  var m = S$RescriptStruct.description(struct);
  if (m !== undefined) {
    Object.assign(schema, description(m));
  }
  var match$2 = S$RescriptStruct.Defaulted.classify(struct);
  if (match$2 !== undefined) {
    var destructingError = S$RescriptStruct.serializeWith(Caml_option.some(match$2._0), struct);
    if (destructingError.TAG === /* Ok */0) {
      Object.assign(schema, $$default(destructingError._0));
    } else {
      raise({
            TAG: /* DefaultDestructingFailed */2,
            destructingErrorMessage: S$RescriptStruct.$$Error.toString(destructingError._0)
          });
    }
  }
  var metadataRawSchema = S$RescriptStruct.Metadata.get(struct, schemaExtendMetadataId);
  if (metadataRawSchema !== undefined) {
    Object.assign(schema, metadataRawSchema);
  }
  return schema;
}

function make(struct) {
  try {
    if (isOptionalStruct(struct)) {
      return raise(/* UnsupportedRootOptional */1);
    }
    var schema = makeStructSchema(struct);
    Object.assign(schema, schemaDialect(undefined));
    return {
            TAG: /* Ok */0,
            _0: schema
          };
  }
  catch (raw_error){
    var error = Caml_js_exceptions.internalToOCamlException(raw_error);
    if (error.RE_EXN_ID === Exception) {
      return {
              TAG: /* Error */1,
              _0: toString(error._1)
            };
    }
    throw error;
  }
}

function extend(struct, schema) {
  var existingSchemaExtend = S$RescriptStruct.Metadata.get(struct, schemaExtendMetadataId);
  return S$RescriptStruct.Metadata.set(struct, schemaExtendMetadataId, existingSchemaExtend !== undefined ? Object.assign({}, existingSchemaExtend, schema) : schema);
}

var Arrayable = JSONSchema7.Arrayable;

var Definition = JSONSchema7.Definition;

var Dependency = JSONSchema7.Dependency;

exports.Arrayable = Arrayable;
exports.Definition = Definition;
exports.Dependency = Dependency;
exports.make = make;
exports.extend = extend;
/* schemaExtendMetadataId Not a pure module */
