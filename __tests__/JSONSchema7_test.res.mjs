// Generated by ReScript, PLEASE EDIT WITH CARE

import Ava from "ava";
import * as JSONSchema7 from "../src/JSONSchema7.res.mjs";

Ava("Arrayable", (function (t) {
        t.deepEqual(JSONSchema7.Arrayable.classify([
                  1,
                  2
                ]), {
              TAG: "Array",
              _0: [
                1,
                2
              ]
            });
        t.deepEqual(JSONSchema7.Arrayable.classify(1), {
              TAG: "Single",
              _0: 1
            });
      }));

Ava("Definition", (function (t) {
        t.deepEqual(JSONSchema7.Definition.classify({
                  title: "foo"
                }), {
              TAG: "Schema",
              _0: {
                title: "foo"
              }
            });
        t.deepEqual(JSONSchema7.Definition.classify(true), {
              TAG: "Boolean",
              _0: true
            });
      }));

Ava("Dependency", (function (t) {
        t.deepEqual({
              title: "foo"
            }, {
              title: "foo"
            });
        t.deepEqual([
              "field1",
              "field2"
            ], [
              "field1",
              "field2"
            ]);
      }));

export {
  
}
/*  Not a pure module */
