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

module PrimitiveCoercionTests = {
  test("Constructs unknown primitive with coercion to the same type", t => {
    let primitive = "  Hello world!"
    let coercedPrimitive = "Hello world!"
    let unknownPrimitive = primitive->unsafeToUnknown

    let struct = S.coercedString(~constructor=value => value->Js.String2.trim->Ok, ())

    t->Assert.deepEqual(struct->S.construct(unknownPrimitive), Ok(coercedPrimitive), ())
    t->Assert.deepEqual(unknownPrimitive->S.constructWith(struct), Ok(coercedPrimitive), ())
  })

  test("Constructs unknown primitive with coercion to another type", t => {
    let primitive = 123
    let coercedPrimitive = 123.
    let unknownPrimitive = primitive->unsafeToUnknown

    let struct = S.coercedInt(~constructor=value => value->Js.Int.toFloat->Ok, ())

    t->Assert.deepEqual(struct->S.construct(unknownPrimitive), Ok(coercedPrimitive), ())
    t->Assert.deepEqual(unknownPrimitive->S.constructWith(struct), Ok(coercedPrimitive), ())
  })

  test("Throws for a CoercedPrimitive factory without either a constructor, or a destructor", t => {
    t->Assert.throws(() => {
      S.coercedString()->ignore
    }, ~expectations=ThrowsException.make(
      ~message="For a Coerced struct either a constructor, or a destructor is required",
      (),
    ), ())
  })

  test("CoercedPrimitive construction fails when constructor isn't provided", t => {
    let primitive = "Hello world!"
    let unknownPrimitive = primitive->unsafeToUnknown

    let struct = S.coercedString(~destructor=value => value->Ok, ())

    t->Assert.deepEqual(
      struct->S.construct(unknownPrimitive),
      Error("Struct missing constructor at root"),
      (),
    )
    t->Assert.deepEqual(
      unknownPrimitive->S.constructWith(struct),
      Error("Struct missing constructor at root"),
      (),
    )
  })

  test("Construction fails when user returns error in a CoercedPrimitive constructor", t => {
    let primitive = "Hello world!"
    let unknownPrimitive = primitive->unsafeToUnknown
    let struct = S.coercedString(~constructor=_ => Error("User error"), ())

    t->Assert.deepEqual(
      struct->S.construct(unknownPrimitive),
      Error("Struct construction failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      unknownPrimitive->S.constructWith(struct),
      Error("Struct construction failed at root. Reason: User error"),
      (),
    )
  })

  test("Destructs unknown record with with coercion to the same type", t => {
    let primitive = "  Hello world!"
    let coercedPrimitive = "Hello world!"
    let unknownCoercedPrimitive = coercedPrimitive->unsafeToUnknown

    let struct = S.coercedString(~destructor=value => value->Js.String2.trim->Ok, ())

    t->Assert.deepEqual(struct->S.destruct(primitive), Ok(unknownCoercedPrimitive), ())
    t->Assert.deepEqual(primitive->S.destructWith(struct), Ok(unknownCoercedPrimitive), ())
  })

  test("Destructs unknown record with with coercion to another type", t => {
    let primitive = 123
    let coercedPrimitive = 123.
    let unknownCoercedPrimitive = coercedPrimitive->unsafeToUnknown

    let struct = S.coercedFloat(~destructor=value => value->Js.Int.toFloat->Ok, ())

    t->Assert.deepEqual(struct->S.destruct(primitive), Ok(unknownCoercedPrimitive), ())
    t->Assert.deepEqual(primitive->S.destructWith(struct), Ok(unknownCoercedPrimitive), ())
  })

  test("CoercedPrimitive destruction fails when destructor isn't provided", t => {
    let primitive = "Hello world!"

    let struct = S.coercedString(~constructor=value => value->Ok, ())

    t->Assert.deepEqual(
      struct->S.destruct(primitive),
      Error("Struct missing destructor at root"),
      (),
    )
    t->Assert.deepEqual(
      primitive->S.destructWith(struct),
      Error("Struct missing destructor at root"),
      (),
    )
  })

  test("Destruction fails when user returns error in a CoercedPrimitive destructor", t => {
    let primitive = "Hello world!"

    let struct = S.coercedString(~destructor=_ => Error("User error"), ())

    t->Assert.deepEqual(
      struct->S.destruct(primitive),
      Error("Struct destruction failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      primitive->S.destructWith(struct),
      Error("Struct destruction failed at root. Reason: User error"),
      (),
    )
  })

  test("Constructs a CoercedPrimitive and destructs it back to the initial state", t => {
    let primitive = 123
    let unknownPrimitive = primitive->unsafeToUnknown

    let struct = S.coercedInt(
      ~constructor=int => int->Js.Int.toFloat->Ok,
      ~destructor=value => value->Belt.Int.fromFloat->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.construct(unknownPrimitive)->Belt.Result.map(record => struct->S.destruct(record)),
      Ok(Ok(unknownPrimitive)),
      (),
    )
  })
}

module RecordCoercionTests = {
  type singleFieldRecord = {foo: string}
  type multipleFieldsRecord = {boo: string, zoo: string}
  type user = {name: string, email: string, age: int}
  type nestedRecord = {nested: singleFieldRecord}
  type optionalNestedRecord = {singleFieldRecord: option<singleFieldRecord>}

  test("Constructs unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(~fields=("foo", S.string()), ~constructor=foo => {foo: foo}->Ok, ())

    t->Assert.deepEqual(struct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(struct), Ok(record), ())
  })

  test("Constructs unknown record with multiple fields", t => {
    let record = {boo: "bar", zoo: "jee"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record2(
      ~fields=(("boo", S.string()), ("zoo", S.string())),
      ~constructor=((boo, zoo)) => {boo: boo, zoo: zoo}->Ok,
      (),
    )

    t->Assert.deepEqual(struct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(struct), Ok(record), ())
  })

  test("Constructs unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->unsafeToUnknown
    let struct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age}->Ok,
      (),
    )

    t->Assert.deepEqual(struct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(struct), Ok(record), ())
  })

  test("Constructs unknown record with optional nested record", t => {
    let recordWithSomeField = {singleFieldRecord: Some({foo: "bar"})}
    let recordWithNoneField = {singleFieldRecord: None}

    let unknownRecordWithSomeField =
      %raw(`{"singleFieldRecord":{"MUST_BE_MAPPED":"bar"}}`)->unsafeToUnknown
    let unknownRecordWithNoneField = %raw(`{}`)->unsafeToUnknown

    let struct = S.record1(
      ~fields=(
        "singleFieldRecord",
        S.option(
          S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~constructor=foo => {foo: foo}->Ok, ()),
        ),
      ),
      ~constructor=singleFieldRecord => {singleFieldRecord: singleFieldRecord}->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.construct(unknownRecordWithSomeField),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithSomeField->S.constructWith(struct),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      struct->S.construct(unknownRecordWithNoneField),
      Ok(recordWithNoneField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithNoneField->S.constructWith(struct),
      Ok(recordWithNoneField),
      (),
    )
  })

  test("Constructs unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->unsafeToUnknown
    let arrayOfRecordsStruct = S.array(
      S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~constructor=foo => {foo: foo}->Ok, ()),
    )

    t->Assert.deepEqual(
      arrayOfRecordsStruct->S.construct(unknownArrayOfRecords),
      Ok(arrayOfRecords),
      (),
    )
    t->Assert.deepEqual(
      unknownArrayOfRecords->S.constructWith(arrayOfRecordsStruct),
      Ok(arrayOfRecords),
      (),
    )
  })

  test("Throws for a Record factory without either a constructor, or a destructor", t => {
    t->Assert.throws(() => {
      S.record1(~fields=("any", S.string()), ())->ignore
    }, ~expectations=ThrowsException.make(
      ~message="For a Record struct either a constructor, or a destructor is required",
      (),
    ), ())
  })

  test("Record construction fails when constructor isn't provided", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ())

    t->Assert.deepEqual(
      struct->S.construct(unknownRecord),
      Error("Struct missing constructor at root"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(struct),
      Error("Struct missing constructor at root"),
      (),
    )
  })

  test("Nested record construction fails when constructor isn't provided", t => {
    let record = {nested: {foo: "bar"}}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ()),
      ),
      ~constructor=nested => {nested: nested}->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.construct(unknownRecord),
      Error(`Struct missing constructor at ."nested"`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(struct),
      Error(`Struct missing constructor at ."nested"`),
      (),
    )
  })

  test("Construction fails when user returns error in a root record constructor", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(~fields=("foo", S.string()), ~constructor=_ => Error("User error"), ())

    t->Assert.deepEqual(
      struct->S.construct(unknownRecord),
      Error("Struct construction failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(struct),
      Error("Struct construction failed at root. Reason: User error"),
      (),
    )
  })

  test("Construction fails when user returns error in a nested record constructor", t => {
    let record = {nested: {foo: "bar"}}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~constructor=_ => Error("User error"), ()),
      ),
      ~constructor=nested => {nested: nested}->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.construct(unknownRecord),
      Error(`Struct construction failed at ."nested". Reason: User error`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(struct),
      Error(`Struct construction failed at ."nested". Reason: User error`),
      (),
    )
  })

  test("Destructs unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ())

    t->Assert.deepEqual(struct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(struct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with multiple fields", t => {
    let record = {boo: "bar", zoo: "jee"}

    let unknownRecord = record->unsafeToUnknown
    let struct = S.record2(
      ~fields=(("boo", S.string()), ("zoo", S.string())),
      ~destructor=({boo, zoo}) => (boo, zoo)->Ok,
      (),
    )

    t->Assert.deepEqual(struct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(struct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->unsafeToUnknown
    let struct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~destructor=({name, email, age}) => (name, email, age)->Ok,
      (),
    )

    t->Assert.deepEqual(struct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(struct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with optional nested record", t => {
    let recordWithSomeField = {singleFieldRecord: Some({foo: "bar"})}
    let recordWithNoneField = {singleFieldRecord: None}

    let unknownRecordWithSomeField =
      %raw(`{"singleFieldRecord":{"MUST_BE_MAPPED":"bar"}}`)->unsafeToUnknown
    let unknownRecordWithNoneField = %raw(`{"singleFieldRecord":undefined}`)->unsafeToUnknown

    let struct = S.record1(
      ~fields=(
        "singleFieldRecord",
        S.option(
          S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~destructor=({foo}) => foo->Ok, ()),
        ),
      ),
      ~destructor=({singleFieldRecord}) => singleFieldRecord->Ok,
      (),
    )

    t->Assert.deepEqual(struct->S.destruct(recordWithSomeField), Ok(unknownRecordWithSomeField), ())
    t->Assert.deepEqual(
      recordWithSomeField->S.destructWith(struct),
      Ok(unknownRecordWithSomeField),
      (),
    )
    t->Assert.deepEqual(struct->S.destruct(recordWithNoneField), Ok(unknownRecordWithNoneField), ())
    t->Assert.deepEqual(
      recordWithNoneField->S.destructWith(struct),
      Ok(unknownRecordWithNoneField),
      (),
    )
  })

  test("Destructs unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->unsafeToUnknown
    let arrayOfRecordsStruct = S.array(
      S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~destructor=({foo}) => foo->Ok, ()),
    )

    t->Assert.deepEqual(
      arrayOfRecordsStruct->S.destruct(arrayOfRecords),
      Ok(unknownArrayOfRecords),
      (),
    )
    t->Assert.deepEqual(
      arrayOfRecords->S.destructWith(arrayOfRecordsStruct),
      Ok(unknownArrayOfRecords),
      (),
    )
  })

  test("Record destruction fails when destructor isn't provided", t => {
    let record = {foo: "bar"}

    let struct = S.record1(~fields=("foo", S.string()), ~constructor=foo => {foo: foo}->Ok, ())

    t->Assert.deepEqual(struct->S.destruct(record), Error("Struct missing destructor at root"), ())
    t->Assert.deepEqual(
      record->S.destructWith(struct),
      Error("Struct missing destructor at root"),
      (),
    )
  })

  test("Nested record destruction fails when destructor isn't provided", t => {
    let record = {nested: {foo: "bar"}}

    let struct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~constructor=foo => {foo: foo}->Ok, ()),
      ),
      ~destructor=({nested}) => nested->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.destruct(record),
      Error(`Struct missing destructor at ."nested"`),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(struct),
      Error(`Struct missing destructor at ."nested"`),
      (),
    )
  })

  test("Destruction fails when user returns error in a root record destructor", t => {
    let record = {foo: "bar"}

    let struct = S.record1(~fields=("foo", S.string()), ~destructor=_ => Error("User error"), ())

    t->Assert.deepEqual(
      struct->S.destruct(record),
      Error("Struct destruction failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(struct),
      Error("Struct destruction failed at root. Reason: User error"),
      (),
    )
  })

  test("Destruction fails when user returns error in a nested record destructor", t => {
    let record = {nested: {foo: "bar"}}

    let struct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~destructor=_ => Error("User error"), ()),
      ),
      ~destructor=({nested}) => nested->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.destruct(record),
      Error(`Struct destruction failed at ."nested". Reason: User error`),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(struct),
      Error(`Struct destruction failed at ."nested". Reason: User error`),
      (),
    )
  })

  test("Constructs a record with fields mapping and destructs it back to the initial state", t => {
    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->unsafeToUnknown
    let struct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age}->Ok,
      ~destructor=({name, email, age}) => (name, email, age)->Ok,
      (),
    )

    t->Assert.deepEqual(
      struct->S.construct(unknownRecord)->Belt.Result.map(record => struct->S.destruct(record)),
      Ok(Ok(unknownRecord)),
      (),
    )
  })
}
