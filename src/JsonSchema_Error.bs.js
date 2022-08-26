'use strict';

var S = require("rescript-struct/src/S.bs.js");

function make(param) {
  return {
          kind: /* UnsupportedOptionalDictItem */0,
          location: []
        };
}

var UnsupportedOptionalDictItem = {
  make: make
};

function make$1(param) {
  return {
          kind: /* UnsupportedOptionalTupleItem */5,
          location: []
        };
}

var UnsupportedOptionalTupleItem = {
  make: make$1
};

function make$2(param) {
  return {
          kind: /* UnsupportedOptionalUnionItem */6,
          location: []
        };
}

var UnsupportedOptionalUnionItem = {
  make: make$2
};

function make$3(struct) {
  return {
          kind: {
            TAG: /* UnsupportedStruct */0,
            _0: S.name(struct)
          },
          location: []
        };
}

var UnsupportedStruct = {
  make: make$3
};

function make$4(param) {
  return {
          kind: /* UnsupportedOptionalArrayItem */1,
          location: []
        };
}

var UnsupportedOptionalArrayItem = {
  make: make$4
};

function make$5(param) {
  return {
          kind: /* UnsupportedOptionalNullItem */4,
          location: []
        };
}

var UnsupportedOptionalNullItem = {
  make: make$5
};

function make$6(param) {
  return {
          kind: /* UnsupportedNestedOptional */2,
          location: []
        };
}

var UnsupportedNestedOptional = {
  make: make$6
};

function make$7(param) {
  return {
          kind: /* UnsupportedRootOptional */3,
          location: []
        };
}

var UnsupportedRootOptional = {
  make: make$7
};

function make$8(destructingErrorMessage) {
  return {
          kind: {
            TAG: /* DefaultDestructingFailed */1,
            destructingErrorMessage: destructingErrorMessage
          },
          location: []
        };
}

var DefaultDestructingFailed = {
  make: make$8
};

function formatLocation($$location) {
  if ($$location.length === 0) {
    return "root";
  } else {
    return $$location.map(function (s) {
                  return "[\"" + s._0 + "\"]";
                }).join("");
  }
}

function prependField(error, field) {
  error.location = [/* Field */{
        _0: field
      }].concat(error.location);
  return error;
}

function toString(error) {
  var locationText = formatLocation(error.location);
  var structName = error.kind;
  var reason;
  if (typeof structName === "number") {
    switch (structName) {
      case /* UnsupportedOptionalDictItem */0 :
          reason = "Optional struct is not supported as Dict item";
          break;
      case /* UnsupportedOptionalArrayItem */1 :
          reason = "Optional struct is not supported as Array item";
          break;
      case /* UnsupportedNestedOptional */2 :
          reason = "Optional struct is not supported inside the Option struct";
          break;
      case /* UnsupportedRootOptional */3 :
          reason = "Optional struct is not supported at root";
          break;
      case /* UnsupportedOptionalNullItem */4 :
          reason = "Optional struct is not supported as Null item";
          break;
      case /* UnsupportedOptionalTupleItem */5 :
          reason = "Optional struct is not supported as Tuple item";
          break;
      case /* UnsupportedOptionalUnionItem */6 :
          reason = "Optional struct is not supported as Union item";
          break;
      
    }
  } else {
    reason = structName.TAG === /* UnsupportedStruct */0 ? "The " + structName._0 + " struct is not supported" : "Couldn't destruct default value. Error: " + structName.destructingErrorMessage + "";
  }
  return "[ReScript JSON Schema] Failed converting at " + locationText + ". Reason: " + reason + "";
}

exports.UnsupportedOptionalDictItem = UnsupportedOptionalDictItem;
exports.UnsupportedOptionalTupleItem = UnsupportedOptionalTupleItem;
exports.UnsupportedOptionalUnionItem = UnsupportedOptionalUnionItem;
exports.UnsupportedStruct = UnsupportedStruct;
exports.UnsupportedOptionalArrayItem = UnsupportedOptionalArrayItem;
exports.UnsupportedNestedOptional = UnsupportedNestedOptional;
exports.UnsupportedOptionalNullItem = UnsupportedOptionalNullItem;
exports.UnsupportedRootOptional = UnsupportedRootOptional;
exports.DefaultDestructingFailed = DefaultDestructingFailed;
exports.prependField = prependField;
exports.toString = toString;
/* S Not a pure module */
