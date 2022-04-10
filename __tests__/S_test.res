open Ava

external unsafeToUnknown: 'unknown => Js.Json.t = "%identity"

test("Constructs unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let struct = S.string()

  t->Assert.deepEqual(struct->S.construct(unknownPrimitive), Ok(primitive), ())
  t->Assert.deepEqual(unknownPrimitive->S.constructWith(struct), Ok(primitive), ())
})

test(
  "Constructs unknown primitive without validation. Note: Use Ajv.parse to safely construct with validation",
  t => {
    let primitivee = 123.

    let unknownPrimitive = Js.Json.number(primitivee)
    let struct = S.string()

    t->Assert.unsafeDeepEqual(struct->S.construct(unknownPrimitive), Ok(primitivee), ())
    t->Assert.unsafeDeepEqual(unknownPrimitive->S.constructWith(struct), Ok(primitivee), ())
  },
)

test("Constructs unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let struct = S.array(S.string())

  t->Assert.deepEqual(struct->S.construct(unknownArrayOfPrimitives), Ok(arrayOfPrimitives), ())
  t->Assert.deepEqual(unknownArrayOfPrimitives->S.constructWith(struct), Ok(arrayOfPrimitives), ())
})

test("Constructs unknown dict of primitives", t => {
  let dictOfPrimitives = Js.Dict.fromArray([("foo", "bar"), ("baz", "qux")])
  let unknownDictOfPrimitives = dictOfPrimitives->unsafeToUnknown

  let struct = S.dict(S.string())

  t->Assert.deepEqual(struct->S.construct(unknownDictOfPrimitives), Ok(dictOfPrimitives), ())
  t->Assert.deepEqual(unknownDictOfPrimitives->S.constructWith(struct), Ok(dictOfPrimitives), ())
})

test("Destructs unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let struct = S.string()

  t->Assert.deepEqual(struct->S.destruct(primitive), Ok(unknownPrimitive), ())
  t->Assert.deepEqual(primitive->S.destructWith(struct), Ok(unknownPrimitive), ())
})

test("Destructs unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let struct = S.array(S.string())

  t->Assert.deepEqual(struct->S.destruct(arrayOfPrimitives), Ok(unknownArrayOfPrimitives), ())
  t->Assert.deepEqual(arrayOfPrimitives->S.destructWith(struct), Ok(unknownArrayOfPrimitives), ())
})

test("Using default value when constructing optional unknown primitive", t => {
  let defaultValue = 123.
  let unknownPrimitive = None->unsafeToUnknown

  let struct =
    S.option(S.coercedInt(~constructor=value => value->Js.Int.toFloat->Ok, ()))->S.default(
      defaultValue,
    )

  t->Assert.deepEqual(struct->S.construct(unknownPrimitive), Ok(defaultValue), ())
  t->Assert.deepEqual(unknownPrimitive->S.constructWith(struct), Ok(defaultValue), ())
})
