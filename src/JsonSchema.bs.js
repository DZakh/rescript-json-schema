'use strict';

var S = require("rescript-struct/src/S.bs.js");
var Curry = require("rescript/lib/js/curry.js");
var Deepmerge = require("deepmerge");
var Caml_option = require("rescript/lib/js/caml_option.js");
var JsonSchema_Error = require("./JsonSchema_Error.bs.js");

function mapi(array, fn) {
  var newArray = [];
  var idxRef = 0;
  var maybeErrorRef;
  while(idxRef < array.length && maybeErrorRef === undefined) {
    var idx = idxRef;
    var item = array[idx];
    var value = fn(item, idx);
    if (value.TAG === /* Ok */0) {
      newArray.push(value._0);
      idxRef = idxRef + 1 | 0;
    } else {
      maybeErrorRef = value;
    }
  };
  var error = maybeErrorRef;
  if (error !== undefined) {
    return error;
  } else {
    return {
            TAG: /* Ok */0,
            _0: newArray
          };
  }
}

var schemaDialect = {
  $schema: "http://json-schema.org/draft-07/schema#"
};

var empty = {};

var string = {
  type: "string"
};

var integer = {
  type: "integer"
};

var number = {
  type: "number"
};

var $$boolean = {
  type: "boolean"
};

function $$null(innerSchema) {
  return {
          anyOf: [
            innerSchema,
            {
              type: "null"
            }
          ]
        };
}

var never = {
  not: {}
};

function array(innerSchema) {
  return {
          items: innerSchema,
          type: "array"
        };
}

function tuple(items) {
  return {
          items: items,
          type: "array",
          minItems: items.length,
          maxItems: items.length
        };
}

function dict(innerSchema) {
  return {
          type: "object",
          additionalProperties: innerSchema
        };
}

function record(properties, additionalProperties, required) {
  var schema = {
    type: "object",
    properties: properties,
    additionalProperties: additionalProperties
  };
  if (required.length !== 0) {
    return Deepmerge(schema, {
                required: required
              });
  } else {
    return schema;
  }
}

var deprecated = {
  deprecated: true
};

function string$1(value) {
  return {
          type: "string",
          const: value
        };
}

function integer$1(value) {
  return {
          type: "integer",
          const: value
        };
}

function number$1(value) {
  return {
          type: "number",
          const: value
        };
}

function $$boolean$1(value) {
  return {
          type: "boolean",
          const: value
        };
}

var $$null$1 = {
  type: "null"
};

var Metadata = S.MakeMetadata({
      namespace: "rescript-json-schema:raw"
    });

function makeNode(struct) {
  var maybeMetadataRawSchema = Curry._1(Metadata.get, struct);
  var fn = function (node) {
    if (maybeMetadataRawSchema !== undefined) {
      return {
              rawSchema: Deepmerge(node.rawSchema, Caml_option.valFromOption(maybeMetadataRawSchema)),
              isRequired: node.isRequired
            };
    } else {
      return node;
    }
  };
  var innerStruct = S.classify(struct);
  var result;
  if (typeof innerStruct === "number") {
    switch (innerStruct) {
      case /* Never */0 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: never,
              isRequired: true
            }
          };
          break;
      case /* Unknown */1 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: empty,
              isRequired: true
            }
          };
          break;
      case /* String */2 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: string,
              isRequired: true
            }
          };
          break;
      case /* Int */3 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: integer,
              isRequired: true
            }
          };
          break;
      case /* Float */4 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: number,
              isRequired: true
            }
          };
          break;
      case /* Bool */5 :
          result = {
            TAG: /* Ok */0,
            _0: {
              rawSchema: $$boolean,
              isRequired: true
            }
          };
          break;
      case /* Date */6 :
          result = {
            TAG: /* Error */1,
            _0: JsonSchema_Error.UnsupportedStruct.make(struct)
          };
          break;
      
    }
  } else {
    switch (innerStruct.TAG | 0) {
      case /* Literal */0 :
          var value = innerStruct._0;
          if (typeof value === "number") {
            switch (value) {
              case /* EmptyNull */0 :
                  result = {
                    TAG: /* Ok */0,
                    _0: {
                      rawSchema: $$null$1,
                      isRequired: true
                    }
                  };
                  break;
              case /* EmptyOption */1 :
              case /* NaN */2 :
                  result = {
                    TAG: /* Error */1,
                    _0: JsonSchema_Error.UnsupportedStruct.make(struct)
                  };
                  break;
              
            }
          } else {
            switch (value.TAG | 0) {
              case /* String */0 :
                  result = {
                    TAG: /* Ok */0,
                    _0: {
                      rawSchema: string$1(value._0),
                      isRequired: true
                    }
                  };
                  break;
              case /* Int */1 :
                  result = {
                    TAG: /* Ok */0,
                    _0: {
                      rawSchema: integer$1(value._0),
                      isRequired: true
                    }
                  };
                  break;
              case /* Float */2 :
                  result = {
                    TAG: /* Ok */0,
                    _0: {
                      rawSchema: number$1(value._0),
                      isRequired: true
                    }
                  };
                  break;
              case /* Bool */3 :
                  result = {
                    TAG: /* Ok */0,
                    _0: {
                      rawSchema: $$boolean$1(value._0),
                      isRequired: true
                    }
                  };
                  break;
              
            }
          }
          break;
      case /* Option */1 :
          var result$1 = makeNode(innerStruct._0);
          if (result$1.TAG === /* Ok */0) {
            var innerNode = result$1._0;
            result = innerNode.isRequired ? ({
                  TAG: /* Ok */0,
                  _0: {
                    rawSchema: innerNode.rawSchema,
                    isRequired: false
                  }
                }) : ({
                  TAG: /* Error */1,
                  _0: JsonSchema_Error.UnsupportedNestedOptional.make(undefined)
                });
          } else {
            result = result$1;
          }
          break;
      case /* Null */2 :
          var result$2 = makeNode(innerStruct._0);
          if (result$2.TAG === /* Ok */0) {
            var innerNode$1 = result$2._0;
            result = innerNode$1.isRequired ? ({
                  TAG: /* Ok */0,
                  _0: {
                    rawSchema: $$null(innerNode$1.rawSchema),
                    isRequired: true
                  }
                }) : ({
                  TAG: /* Error */1,
                  _0: JsonSchema_Error.UnsupportedOptionalNullItem.make(undefined)
                });
          } else {
            result = result$2;
          }
          break;
      case /* Array */3 :
          var result$3 = makeNode(innerStruct._0);
          if (result$3.TAG === /* Ok */0) {
            var innerNode$2 = result$3._0;
            result = innerNode$2.isRequired ? ({
                  TAG: /* Ok */0,
                  _0: {
                    rawSchema: array(innerNode$2.rawSchema),
                    isRequired: true
                  }
                }) : ({
                  TAG: /* Error */1,
                  _0: JsonSchema_Error.UnsupportedOptionalDictItem.make(undefined)
                });
          } else {
            result = result$3;
          }
          break;
      case /* Record */4 :
          var unknownKeys = innerStruct.unknownKeys;
          var fieldNames = innerStruct.fieldNames;
          var fields = innerStruct.fields;
          var fn$1 = function (fieldNodes) {
            var properties = {};
            var required = [];
            fieldNodes.forEach(function (fieldNode, idx) {
                  var fieldName = fieldNames[idx];
                  if (fieldNode.isRequired) {
                    required.push(fieldName);
                  }
                  properties[fieldName] = fieldNode.rawSchema;
                });
            var rawSchema = record(properties, unknownKeys ? true : false, required);
            return {
                    rawSchema: rawSchema,
                    isRequired: true
                  };
          };
          var result$4 = mapi(fieldNames, (function (fieldName, param) {
                  var fieldStruct = fields[fieldName];
                  var result = makeNode(fieldStruct);
                  if (result.TAG === /* Ok */0) {
                    return result;
                  } else {
                    return {
                            TAG: /* Error */1,
                            _0: JsonSchema_Error.prependField(result._0, fieldName)
                          };
                  }
                }));
          result = result$4.TAG === /* Ok */0 ? ({
                TAG: /* Ok */0,
                _0: fn$1(result$4._0)
              }) : result$4;
          break;
      case /* Tuple */5 :
          var result$5 = mapi(innerStruct._0, (function (innerStruct, idx) {
                  var result = makeNode(innerStruct);
                  var result$1;
                  if (result.TAG === /* Ok */0) {
                    var innerNode = result._0;
                    result$1 = innerNode.isRequired ? ({
                          TAG: /* Ok */0,
                          _0: innerNode.rawSchema
                        }) : ({
                          TAG: /* Error */1,
                          _0: JsonSchema_Error.UnsupportedOptionalDictItem.make(undefined)
                        });
                  } else {
                    result$1 = result;
                  }
                  if (result$1.TAG === /* Ok */0) {
                    return result$1;
                  } else {
                    return {
                            TAG: /* Error */1,
                            _0: JsonSchema_Error.prependField(result$1._0, idx.toString())
                          };
                  }
                }));
          result = result$5.TAG === /* Ok */0 ? ({
                TAG: /* Ok */0,
                _0: {
                  rawSchema: tuple(result$5._0),
                  isRequired: true
                }
              }) : result$5;
          break;
      case /* Union */6 :
          var fn$2 = function (items) {
            return {
                    rawSchema: {
                      anyOf: items
                    },
                    isRequired: true
                  };
          };
          var result$6 = mapi(innerStruct._0, (function (innerStruct, idx) {
                  var result = makeNode(innerStruct);
                  var result$1;
                  if (result.TAG === /* Ok */0) {
                    var innerNode = result._0;
                    result$1 = innerNode.isRequired ? ({
                          TAG: /* Ok */0,
                          _0: innerNode.rawSchema
                        }) : ({
                          TAG: /* Error */1,
                          _0: JsonSchema_Error.UnsupportedOptionalUnionItem.make(undefined)
                        });
                  } else {
                    result$1 = result;
                  }
                  if (result$1.TAG === /* Ok */0) {
                    return result$1;
                  } else {
                    return {
                            TAG: /* Error */1,
                            _0: JsonSchema_Error.prependField(result$1._0, idx.toString())
                          };
                  }
                }));
          result = result$6.TAG === /* Ok */0 ? ({
                TAG: /* Ok */0,
                _0: fn$2(result$6._0)
              }) : result$6;
          break;
      case /* Dict */7 :
          var result$7 = makeNode(innerStruct._0);
          if (result$7.TAG === /* Ok */0) {
            var innerNode$3 = result$7._0;
            result = innerNode$3.isRequired ? ({
                  TAG: /* Ok */0,
                  _0: {
                    rawSchema: dict(innerNode$3.rawSchema),
                    isRequired: true
                  }
                }) : ({
                  TAG: /* Error */1,
                  _0: JsonSchema_Error.UnsupportedOptionalDictItem.make(undefined)
                });
          } else {
            result = result$7;
          }
          break;
      case /* Deprecated */8 :
          var maybeMessage = innerStruct.maybeMessage;
          var result$8 = makeNode(innerStruct.struct);
          if (result$8.TAG === /* Ok */0) {
            var innerNode$4 = result$8._0;
            var rawSchema$p = Deepmerge(innerNode$4.rawSchema, deprecated);
            var rawSchema = maybeMessage !== undefined ? Deepmerge(rawSchema$p, {
                    description: maybeMessage
                  }) : rawSchema$p;
            result = {
              TAG: /* Ok */0,
              _0: {
                rawSchema: rawSchema,
                isRequired: false
              }
            };
          } else {
            result = result$8;
          }
          break;
      case /* Default */9 :
          var innerStruct$1 = innerStruct.struct;
          var destructingError = S.serializeWith(Caml_option.some(innerStruct.value), innerStruct$1);
          if (destructingError.TAG === /* Ok */0) {
            var destructedValue = destructingError._0;
            var fn$3 = function (innerNode) {
              return {
                      rawSchema: Deepmerge(innerNode.rawSchema, {
                            default: destructedValue
                          }),
                      isRequired: false
                    };
            };
            var result$9 = makeNode(innerStruct$1);
            result = result$9.TAG === /* Ok */0 ? ({
                  TAG: /* Ok */0,
                  _0: fn$3(result$9._0)
                }) : result$9;
          } else {
            result = {
              TAG: /* Error */1,
              _0: JsonSchema_Error.DefaultDestructingFailed.make(S.$$Error.toString(destructingError._0))
            };
          }
          break;
      
    }
  }
  if (result.TAG === /* Ok */0) {
    return {
            TAG: /* Ok */0,
            _0: fn(result._0)
          };
  } else {
    return result;
  }
}

function make(struct) {
  var result = makeNode(struct);
  var result$1;
  if (result.TAG === /* Ok */0) {
    var node = result._0;
    result$1 = node.isRequired ? ({
          TAG: /* Ok */0,
          _0: Deepmerge(node.rawSchema, schemaDialect)
        }) : ({
          TAG: /* Error */1,
          _0: JsonSchema_Error.UnsupportedRootOptional.make(undefined)
        });
  } else {
    result$1 = result;
  }
  if (result$1.TAG === /* Ok */0) {
    return result$1;
  } else {
    return {
            TAG: /* Error */1,
            _0: JsonSchema_Error.toString(result$1._0)
          };
  }
}

function raw(struct, providedRawSchema) {
  var existingRawSchema = Curry._1(Metadata.get, struct);
  var rawSchema = existingRawSchema !== undefined ? Deepmerge(Caml_option.valFromOption(existingRawSchema), providedRawSchema) : providedRawSchema;
  return Curry._2(Metadata.set, struct, rawSchema);
}

function description(struct, value) {
  return raw(struct, {
              description: value
            });
}

exports.make = make;
exports.raw = raw;
exports.description = description;
/* Metadata Not a pure module */
