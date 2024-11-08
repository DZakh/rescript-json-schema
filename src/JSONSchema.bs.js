// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_dict = require("rescript/lib/js/js_dict.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var JSONSchema7 = require("./JSONSchema7.bs.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");
var S$RescriptSchema = require("rescript-schema/src/S.bs.js");
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

function raise$1(schema) {
  return raise({
              TAG: "UnsupportedOptionalItem",
              _0: schema.n()
            });
}

function raise$2(schema) {
  return raise({
              TAG: "UnsupportedSchema",
              _0: schema.n()
            });
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
  var schemaName = error.code;
  var reason;
  if (typeof schemaName !== "object") {
    reason = schemaName === "UnsupportedNestedOptional" ? "Optional schema is not supported inside the Option schema" : "Optional schema is not supported at root";
  } else {
    switch (schemaName.TAG) {
      case "UnsupportedOptionalItem" :
          reason = "Optional schema is not supported as " + schemaName._0 + " item";
          break;
      case "UnsupportedSchema" :
          reason = "The " + schemaName._0 + " schema is not supported";
          break;
      case "DefaultDestructingFailed" :
          reason = "Couldn't destruct default value. Error: " + schemaName.destructingErrorMessage;
          break;
      
    }
  }
  return "[ReScript JSON Schema] Failed converting at " + pathText + ". Reason: " + reason;
}

var schemaExtendMetadataId = S$RescriptSchema.Metadata.Id.make("rescript-json-schema", "schemaExtend");

function isOptionalSchema(schema) {
  var match = S$RescriptSchema.classify(schema);
  if (typeof match !== "object" || match.TAG !== "Option") {
    return false;
  } else {
    return true;
  }
}

function fromRescriptSchema(schema) {
  var jsonSchema = {};
  var childSchema = S$RescriptSchema.classify(schema);
  if (typeof childSchema !== "object") {
    switch (childSchema) {
      case "Never" :
          jsonSchema.not = {};
          break;
      case "Unknown" :
          break;
      case "String" :
          jsonSchema.type = "string";
          S$RescriptSchema.$$String.refinements(schema).forEach(function (refinement) {
                var match = refinement.kind;
                if (typeof match !== "object") {
                  switch (match) {
                    case "Email" :
                        jsonSchema.format = "email";
                        return ;
                    case "Uuid" :
                        jsonSchema.format = "uuid";
                        return ;
                    case "Cuid" :
                        return ;
                    case "Url" :
                        jsonSchema.format = "uri";
                        return ;
                    case "Datetime" :
                        jsonSchema.format = "date-time";
                        return ;
                    
                  }
                } else {
                  switch (match.TAG) {
                    case "Min" :
                        jsonSchema.minLength = match.length;
                        return ;
                    case "Max" :
                        jsonSchema.maxLength = match.length;
                        return ;
                    case "Length" :
                        var length = match.length;
                        jsonSchema.minLength = length;
                        jsonSchema.maxLength = length;
                        return ;
                    case "Pattern" :
                        jsonSchema.pattern = String(match.re);
                        return ;
                    
                  }
                }
              });
          break;
      case "Int" :
          jsonSchema.type = "integer";
          S$RescriptSchema.Int.refinements(schema).forEach(function (refinement) {
                var match = refinement.kind;
                if (typeof match !== "object") {
                  return ;
                } else {
                  if (match.TAG === "Min") {
                    jsonSchema.minimum = match.value;
                  } else {
                    jsonSchema.maximum = match.value;
                  }
                  return ;
                }
              });
          break;
      case "Float" :
          jsonSchema.type = "number";
          S$RescriptSchema.Float.refinements(schema).forEach(function (refinement) {
                var match = refinement.kind;
                if (match.TAG === "Min") {
                  jsonSchema.minimum = match.value;
                } else {
                  jsonSchema.maximum = match.value;
                }
              });
          break;
      case "Bool" :
          jsonSchema.type = "boolean";
          break;
      
    }
  } else {
    switch (childSchema.TAG) {
      case "Literal" :
          var match = childSchema._0;
          switch (match.kind) {
            case "String" :
                jsonSchema.type = "string";
                jsonSchema.const = match.value;
                break;
            case "Number" :
                var value = match.value;
                var isInt = value % 1 === 0;
                jsonSchema.type = Caml_option.some(isInt ? "integer" : "number");
                jsonSchema.const = value;
                break;
            case "Boolean" :
                jsonSchema.type = "boolean";
                jsonSchema.const = match.value;
                break;
            case "Null" :
                jsonSchema.type = "null";
                break;
            default:
              raise$2(schema);
          }
          break;
      case "Option" :
          var childSchema$1 = childSchema._0;
          if (isOptionalSchema(childSchema$1)) {
            raise("UnsupportedNestedOptional");
          }
          var childJsonSchema = fromRescriptSchema(childSchema$1);
          Object.assign(jsonSchema, childJsonSchema);
          var $$default = S$RescriptSchema.$$Option.$$default(schema);
          if ($$default !== undefined) {
            var defaultValue;
            defaultValue = $$default.TAG === "Value" ? $$default._0 : $$default._0();
            var destructingError = S$RescriptSchema.serializeWith(Caml_option.some(defaultValue), childSchema$1);
            if (destructingError.TAG === "Ok") {
              jsonSchema.default = destructingError._0;
            } else {
              raise({
                    TAG: "DefaultDestructingFailed",
                    destructingErrorMessage: S$RescriptSchema.$$Error.message(destructingError._0)
                  });
            }
          }
          break;
      case "Null" :
          jsonSchema.anyOf = [
            fromRescriptSchema(childSchema._0),
            {
              type: "null"
            }
          ];
          break;
      case "Array" :
          var childSchema$2 = childSchema._0;
          if (isOptionalSchema(childSchema$2)) {
            raise$1(schema);
          }
          jsonSchema.items = Caml_option.some(fromRescriptSchema(childSchema$2));
          jsonSchema.type = "array";
          S$RescriptSchema.$$Array.refinements(schema).forEach(function (refinement) {
                var match = refinement.kind;
                switch (match.TAG) {
                  case "Min" :
                      jsonSchema.minItems = match.length;
                      return ;
                  case "Max" :
                      jsonSchema.maxItems = match.length;
                      return ;
                  case "Length" :
                      var length = match.length;
                      jsonSchema.maxItems = length;
                      jsonSchema.minItems = length;
                      return ;
                  
                }
              });
          break;
      case "Object" :
          var properties = {};
          var required = [];
          childSchema.items.forEach(function (item) {
                var fieldSchema;
                try {
                  fieldSchema = fromRescriptSchema(item.t);
                }
                catch (raw_error){
                  var error = Caml_js_exceptions.internalToOCamlException(raw_error);
                  if (error.RE_EXN_ID === Exception) {
                    throw {
                          RE_EXN_ID: Exception,
                          _1: prependLocation(error._1, item.l),
                          Error: new Error()
                        };
                  }
                  throw error;
                }
                if (!isOptionalSchema(item.t)) {
                  required.push(item.l);
                }
                properties[item.l] = fieldSchema;
              });
          var additionalProperties;
          additionalProperties = childSchema.unknownKeys === "Strip" ? true : false;
          jsonSchema.type = "object";
          jsonSchema.properties = properties;
          jsonSchema.additionalProperties = Caml_option.some(additionalProperties);
          if (required.length !== 0) {
            jsonSchema.required = required;
          }
          break;
      case "Tuple" :
          var items = childSchema.items.map(function (item, idx) {
                try {
                  if (isOptionalSchema(item.t)) {
                    return raise$1(schema);
                  } else {
                    return fromRescriptSchema(item.t);
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
              });
          var itemsNumber = items.length;
          jsonSchema.items = Caml_option.some(items);
          jsonSchema.type = "array";
          jsonSchema.minItems = itemsNumber;
          jsonSchema.maxItems = itemsNumber;
          break;
      case "Union" :
          var items$1 = childSchema._0.map(function (childSchema) {
                if (isOptionalSchema(childSchema)) {
                  return raise$1(schema);
                } else {
                  return fromRescriptSchema(childSchema);
                }
              });
          jsonSchema.anyOf = items$1;
          break;
      case "Dict" :
          var childSchema$3 = childSchema._0;
          if (isOptionalSchema(childSchema$3)) {
            raise$1(schema);
          }
          jsonSchema.type = "object";
          jsonSchema.additionalProperties = Caml_option.some(fromRescriptSchema(childSchema$3));
          break;
      case "JSON" :
          break;
      
    }
  }
  var m = S$RescriptSchema.description(schema);
  if (m !== undefined) {
    jsonSchema.description = m;
  }
  var message = S$RescriptSchema.deprecation(schema);
  if (message !== undefined) {
    Object.assign(jsonSchema, {
          deprecated: true,
          description: message
        });
  }
  var metadataRawSchema = S$RescriptSchema.Metadata.get(schema, schemaExtendMetadataId);
  if (metadataRawSchema !== undefined) {
    Object.assign(jsonSchema, metadataRawSchema);
  }
  return jsonSchema;
}

function make(schema) {
  try {
    if (isOptionalSchema(schema)) {
      return raise("UnsupportedRootOptional");
    }
    var jsonSchema = fromRescriptSchema(schema);
    jsonSchema.$schema = "http://json-schema.org/draft-07/schema#";
    return {
            TAG: "Ok",
            _0: jsonSchema
          };
  }
  catch (raw_error){
    var error = Caml_js_exceptions.internalToOCamlException(raw_error);
    if (error.RE_EXN_ID === Exception) {
      return {
              TAG: "Error",
              _0: toString(error._1)
            };
    }
    throw error;
  }
}

function extend(schema, jsonSchema) {
  var existingSchemaExtend = S$RescriptSchema.Metadata.get(schema, schemaExtendMetadataId);
  return S$RescriptSchema.Metadata.set(schema, schemaExtendMetadataId, existingSchemaExtend !== undefined ? Object.assign({}, existingSchemaExtend, jsonSchema) : jsonSchema);
}

function example(schema, example$1) {
  var newExamples = [S$RescriptSchema.serializeOrRaiseWith(example$1, schema)];
  var existingSchemaExtend = S$RescriptSchema.Metadata.get(schema, schemaExtendMetadataId);
  var schemaExtend;
  if (existingSchemaExtend !== undefined) {
    var examples = existingSchemaExtend.examples;
    schemaExtend = Object.assign({}, existingSchemaExtend, {
          examples: examples !== undefined ? examples.concat(newExamples) : newExamples
        });
  } else {
    schemaExtend = {
      examples: newExamples
    };
  }
  return S$RescriptSchema.Metadata.set(schema, schemaExtendMetadataId, schemaExtend);
}

function primitiveToSchema(primitive) {
  return S$RescriptSchema.literal(primitive);
}

function toIntSchema(jsonSchema) {
  var minimum = jsonSchema.minimum;
  var schema;
  if (minimum !== undefined) {
    schema = S$RescriptSchema.intMin(S$RescriptSchema.$$int, minimum | 0, undefined);
  } else {
    var exclusiveMinimum = jsonSchema.exclusiveMinimum;
    schema = exclusiveMinimum !== undefined ? S$RescriptSchema.intMin(S$RescriptSchema.$$int, exclusiveMinimum + 1 | 0, undefined) : S$RescriptSchema.$$int;
  }
  var maximum = jsonSchema.maximum;
  if (maximum !== undefined) {
    return S$RescriptSchema.intMax(schema, maximum | 0, undefined);
  }
  var exclusiveMinimum$1 = jsonSchema.exclusiveMinimum;
  if (exclusiveMinimum$1 !== undefined) {
    return S$RescriptSchema.intMax(schema, exclusiveMinimum$1 - 1 | 0, undefined);
  } else {
    return schema;
  }
}

function definitionToDefaultValue(definition) {
  var s = JSONSchema7.Definition.classify(definition);
  if (s.TAG === "Schema") {
    return s._0.default;
  }
  
}

function toRescriptSchema(jsonSchema) {
  var anySchema = S$RescriptSchema.json(false);
  var definitionToSchema = function (definition) {
    var s = JSONSchema7.Definition.classify(definition);
    if (s.TAG === "Schema") {
      return toRescriptSchema(s._0);
    } else {
      return anySchema;
    }
  };
  var type_ = jsonSchema.type;
  var schema;
  var exit = 0;
  var exit$1 = 0;
  if (jsonSchema.nullable) {
    schema = S$RescriptSchema.$$null(toRescriptSchema(Object.assign({}, jsonSchema, {
                  nullable: false
                })));
  } else if (type_ !== undefined) {
    var type_$1 = Caml_option.valFromOption(type_);
    if (type_$1 === "object") {
      var properties = jsonSchema.properties;
      if (properties !== undefined) {
        var schema$1 = S$RescriptSchema.object(function (s) {
              return Js_dict.fromArray(Js_dict.entries(properties).map(function (param) {
                              var property = param[1];
                              var key = param[0];
                              var propertySchema = definitionToSchema(property);
                              var r = jsonSchema.required;
                              var propertySchema$1;
                              var exit = 0;
                              if (r !== undefined && r.includes(key)) {
                                propertySchema$1 = propertySchema;
                              } else {
                                exit = 1;
                              }
                              if (exit === 1) {
                                var defaultValue = definitionToDefaultValue(property);
                                propertySchema$1 = defaultValue !== undefined ? S$RescriptSchema.$$Option.getOr(S$RescriptSchema.option(propertySchema), defaultValue) : S$RescriptSchema.option(propertySchema);
                              }
                              return [
                                      key,
                                      s.f(key, propertySchema$1)
                                    ];
                            }));
            });
        var additionalProperties = jsonSchema.additionalProperties;
        schema = additionalProperties !== undefined && Caml_option.valFromOption(additionalProperties) === false ? S$RescriptSchema.$$Object.strict(schema$1) : schema$1;
      } else {
        var additionalProperties$1 = jsonSchema.additionalProperties;
        if (additionalProperties$1 !== undefined) {
          var s = JSONSchema7.Definition.classify(Caml_option.valFromOption(additionalProperties$1));
          schema = s.TAG === "Schema" ? S$RescriptSchema.dict(toRescriptSchema(s._0)) : (
              s._0 ? S$RescriptSchema.dict(anySchema) : S$RescriptSchema.$$Object.strict(S$RescriptSchema.object(function (param) {
                          
                        }))
            );
        } else {
          schema = S$RescriptSchema.object(function (param) {
                
              });
        }
      }
    } else if (type_$1 === "array") {
      var items = jsonSchema.items;
      var schema$2;
      if (items !== undefined) {
        var single = JSONSchema7.Arrayable.classify(Caml_option.valFromOption(items));
        if (single.TAG === "Single") {
          schema$2 = S$RescriptSchema.array(definitionToSchema(single._0));
        } else {
          var array = single._0;
          schema$2 = S$RescriptSchema.tuple(function (s) {
                return array.map(function (d, idx) {
                            return s.item(idx, definitionToSchema(d));
                          });
              });
        }
      } else {
        schema$2 = S$RescriptSchema.array(anySchema);
      }
      var min = jsonSchema.minItems;
      var schema$3 = min !== undefined ? S$RescriptSchema.arrayMinLength(schema$2, min, undefined) : schema$2;
      var max = jsonSchema.maxItems;
      schema = max !== undefined ? S$RescriptSchema.arrayMaxLength(schema$3, max, undefined) : schema$3;
    } else {
      exit$1 = 2;
    }
  } else {
    exit$1 = 2;
  }
  if (exit$1 === 2) {
    var primitives = jsonSchema.enum;
    var definitions = jsonSchema.allOf;
    var definitions$1 = jsonSchema.anyOf;
    if (definitions$1 !== undefined) {
      var len = definitions$1.length;
      if (len !== 1) {
        schema = len !== 0 ? S$RescriptSchema.union(definitions$1.map(definitionToSchema)) : anySchema;
      } else {
        var d = definitions$1[0];
        schema = definitionToSchema(d);
      }
    } else if (definitions !== undefined) {
      var len$1 = definitions.length;
      if (len$1 !== 1) {
        schema = len$1 !== 0 ? S$RescriptSchema.refine(anySchema, (function (s) {
                  return function (data) {
                    definitions.forEach(function (d) {
                          var match = S$RescriptSchema.parseWith(data, definitionToSchema(d));
                          if (match.TAG === "Ok") {
                            return ;
                          } else {
                            return s.fail("Should pass for all schemas of the allOf property.", undefined);
                          }
                        });
                  };
                })) : anySchema;
      } else {
        var d$1 = definitions[0];
        schema = definitionToSchema(d$1);
      }
    } else {
      var definitions$2 = jsonSchema.oneOf;
      if (definitions$2 !== undefined) {
        var len$2 = definitions$2.length;
        if (len$2 !== 1) {
          schema = len$2 !== 0 ? S$RescriptSchema.refine(anySchema, (function (s) {
                    return function (data) {
                      var hasOneValidRef = {
                        contents: false
                      };
                      definitions$2.forEach(function (d) {
                            var match = S$RescriptSchema.parseWith(data, definitionToSchema(d));
                            if (match.TAG === "Ok") {
                              if (hasOneValidRef.contents) {
                                return s.fail("Should pass single schema according to the oneOf property.", undefined);
                              } else {
                                hasOneValidRef.contents = true;
                                return ;
                              }
                            }
                            
                          });
                      if (!hasOneValidRef.contents) {
                        return s.fail("Should pass at least one schema according to the oneOf property.", undefined);
                      }
                      
                    };
                  })) : anySchema;
        } else {
          var d$2 = definitions$2[0];
          schema = definitionToSchema(d$2);
        }
      } else {
        var not = jsonSchema.not;
        if (not !== undefined) {
          var not$1 = Caml_option.valFromOption(not);
          schema = S$RescriptSchema.refine(anySchema, (function (s) {
                  return function (data) {
                    var match = S$RescriptSchema.parseWith(data, definitionToSchema(not$1));
                    if (match.TAG === "Ok") {
                      return s.fail("Should NOT be valid against schema in the not property.", undefined);
                    }
                    
                  };
                }));
        } else if (primitives !== undefined) {
          var len$3 = primitives.length;
          if (len$3 !== 1) {
            schema = len$3 !== 0 ? S$RescriptSchema.union(primitives.map(primitiveToSchema)) : anySchema;
          } else {
            var p = primitives[0];
            schema = S$RescriptSchema.literal(p);
          }
        } else {
          var $$const = jsonSchema.const;
          if ($$const !== undefined) {
            schema = S$RescriptSchema.literal($$const);
          } else if (type_ !== undefined) {
            var match = jsonSchema.multipleOf;
            var type_$2 = Caml_option.valFromOption(type_);
            var exit$2 = 0;
            var exit$3 = 0;
            var match$1 = jsonSchema.format;
            if (Array.isArray(type_$2)) {
              schema = S$RescriptSchema.union(type_$2.map(function (type_) {
                        return toRescriptSchema(Object.assign({}, jsonSchema, {
                                        type: Caml_option.some(type_)
                                      }));
                      }));
            } else if (type_$2 === "string") {
              var pattern = jsonSchema.pattern;
              var schema$4 = pattern !== undefined ? S$RescriptSchema.pattern(S$RescriptSchema.string, new RegExp(pattern), undefined) : S$RescriptSchema.string;
              var minLength = jsonSchema.minLength;
              var schema$5 = minLength !== undefined ? S$RescriptSchema.stringMinLength(schema$4, minLength, undefined) : schema$4;
              var maxLength = jsonSchema.maxLength;
              var schema$6 = maxLength !== undefined ? S$RescriptSchema.stringMaxLength(schema$5, maxLength, undefined) : schema$5;
              var match$2 = jsonSchema.format;
              if (match$2 !== undefined) {
                switch (match$2) {
                  case "date-time" :
                      schema = S$RescriptSchema.datetime(schema$6, undefined);
                      break;
                  case "email" :
                      schema = S$RescriptSchema.email(schema$6, undefined);
                      break;
                  case "uri" :
                      schema = S$RescriptSchema.url(schema$6, undefined);
                      break;
                  case "uuid" :
                      schema = S$RescriptSchema.uuid(schema$6, undefined);
                      break;
                  default:
                    schema = schema$6;
                }
              } else {
                schema = schema$6;
              }
            } else if (type_$2 === "integer" || match$1 !== undefined && match$1 === "int64" && type_$2 === "number") {
              schema = toIntSchema(jsonSchema);
            } else {
              exit$3 = 4;
            }
            if (exit$3 === 4) {
              if (match !== undefined && !(match !== 1 || type_$2 !== "number")) {
                schema = toIntSchema(jsonSchema);
              } else {
                exit$2 = 3;
              }
            }
            if (exit$2 === 3) {
              if (type_$2 === "number") {
                var minimum = jsonSchema.minimum;
                var schema$7;
                if (minimum !== undefined) {
                  schema$7 = S$RescriptSchema.floatMin(S$RescriptSchema.$$float, minimum, undefined);
                } else {
                  var exclusiveMinimum = jsonSchema.exclusiveMinimum;
                  schema$7 = exclusiveMinimum !== undefined ? S$RescriptSchema.floatMin(S$RescriptSchema.$$float, exclusiveMinimum + 1, undefined) : S$RescriptSchema.$$float;
                }
                var maximum = jsonSchema.maximum;
                if (maximum !== undefined) {
                  schema = S$RescriptSchema.floatMax(schema$7, maximum, undefined);
                } else {
                  var exclusiveMinimum$1 = jsonSchema.exclusiveMinimum;
                  schema = exclusiveMinimum$1 !== undefined ? S$RescriptSchema.floatMax(schema$7, exclusiveMinimum$1 - 1, undefined) : schema$7;
                }
              } else if (type_$2 === "boolean") {
                schema = S$RescriptSchema.bool;
              } else if (type_$2 === "null") {
                schema = S$RescriptSchema.literal(null);
              } else {
                exit = 1;
              }
            }
            
          } else {
            exit = 1;
          }
        }
      }
    }
  }
  if (exit === 1) {
    var if_ = jsonSchema.if;
    if (if_ !== undefined) {
      var then = jsonSchema.then;
      if (then !== undefined) {
        var else_ = jsonSchema.else;
        if (else_ !== undefined) {
          var ifSchema = definitionToSchema(Caml_option.valFromOption(if_));
          var thenSchema = definitionToSchema(Caml_option.valFromOption(then));
          var elseSchema = definitionToSchema(Caml_option.valFromOption(else_));
          schema = S$RescriptSchema.refine(anySchema, (function (param) {
                  return function (data) {
                    var match = S$RescriptSchema.parseWith(data, ifSchema);
                    var result;
                    result = match.TAG === "Ok" ? S$RescriptSchema.parseWith(data, thenSchema) : S$RescriptSchema.parseWith(data, elseSchema);
                    if (result.TAG === "Ok") {
                      return ;
                    } else {
                      return S$RescriptSchema.$$Error.raise(result._0);
                    }
                  };
                }));
        } else {
          schema = anySchema;
        }
      } else {
        schema = anySchema;
      }
    } else {
      schema = anySchema;
    }
  }
  var description = jsonSchema.description;
  var schema$8 = description !== undefined ? S$RescriptSchema.describe(schema, description) : schema;
  var description$1 = jsonSchema.description;
  if (description$1 !== undefined) {
    return S$RescriptSchema.describe(schema$8, description$1);
  } else {
    return schema$8;
  }
}

var Arrayable = JSONSchema7.Arrayable;

var Mutable = JSONSchema7.Mutable;

var Definition = JSONSchema7.Definition;

var Dependency = JSONSchema7.Dependency;

exports.Arrayable = Arrayable;
exports.Mutable = Mutable;
exports.Definition = Definition;
exports.Dependency = Dependency;
exports.make = make;
exports.extend = extend;
exports.example = example;
exports.toRescriptSchema = toRescriptSchema;
/* schemaExtendMetadataId Not a pure module */
