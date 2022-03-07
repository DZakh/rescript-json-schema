open Ava

test("Constructs unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let primitiveStruct = S.string()

  t->Assert.deepEqual(primitiveStruct->S.construct(unknownPrimitive), Ok(primitive), ())
  t->Assert.deepEqual(unknownPrimitive->S.constructWith(primitiveStruct), Ok(primitive), ())
})

test(
  "Constructs unknown primitive without validation. Note: Use Ajv.parse to safely construct with validation",
  t => {
    let primitive = 123.

    let unknownPrimitive = Js.Json.number(primitive)
    let primitiveStruct = S.string()

    t->Assert.deepEqual(primitiveStruct->S.construct(unknownPrimitive), Ok(primitive), ())
    t->Assert.deepEqual(unknownPrimitive->S.constructWith(primitiveStruct), Ok(primitive), ())
  },
)

test("Constructs unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let arrayOfPrimitivesStruct = S.array(S.string())

  t->Assert.deepEqual(
    arrayOfPrimitivesStruct->S.construct(unknownArrayOfPrimitives),
    Ok(arrayOfPrimitives),
    (),
  )
  t->Assert.deepEqual(
    unknownArrayOfPrimitives->S.constructWith(arrayOfPrimitivesStruct),
    Ok(arrayOfPrimitives),
    (),
  )
})

test("Destructs unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let primitiveStruct = S.string()

  t->Assert.deepEqual(primitiveStruct->S.destruct(primitive), Ok(unknownPrimitive), ())
  t->Assert.deepEqual(primitive->S.destructWith(primitiveStruct), Ok(unknownPrimitive), ())
})

test("Destructs unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let arrayOfPrimitivesStruct = S.array(S.string())

  t->Assert.deepEqual(
    arrayOfPrimitivesStruct->S.destruct(arrayOfPrimitives),
    Ok(unknownArrayOfPrimitives),
    (),
  )
  t->Assert.deepEqual(
    arrayOfPrimitives->S.destructWith(arrayOfPrimitivesStruct),
    Ok(unknownArrayOfPrimitives),
    (),
  )
})

module RecordConstructingAndDestructingTests = {
  type singleFieldRecord = {foo: string}
  type multipleFieldsRecord = {boo: string, zoo: string}
  type user = {name: string, email: string, age: int}
  type nestedRecord = {nested: singleFieldRecord}
  type optionalNestedRecord = {singleFieldRecord: option<singleFieldRecord>}

  test("Constructs unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~constructor=foo => {foo: foo}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(recordStruct), Ok(record), ())
  })

  test("Constructs unknown record with multiple fields", t => {
    let record = {boo: "bar", zoo: "jee"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record2(
      ~fields=(("boo", S.string()), ("zoo", S.string())),
      ~constructor=((boo, zoo)) => {boo: boo, zoo: zoo}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(recordStruct), Ok(record), ())
  })

  test("Constructs unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.construct(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.constructWith(recordStruct), Ok(record), ())
  })

  test("Constructs unknown record with optional nested record", t => {
    let recordWithSomeField = {singleFieldRecord: Some({foo: "bar"})}
    let recordWithNoneField = {singleFieldRecord: None}

    let unknownRecordWithSomeField =
      %raw(`{"singleFieldRecord":{"MUST_BE_MAPPED":"bar"}}`)->S.unsafeToUnknown
    let unknownRecordWithNoneField = %raw(`{}`)->S.unsafeToUnknown

    let recordStruct = S.record1(
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
      recordStruct->S.construct(unknownRecordWithSomeField),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithSomeField->S.constructWith(recordStruct),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      recordStruct->S.construct(unknownRecordWithNoneField),
      Ok(recordWithNoneField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithNoneField->S.constructWith(recordStruct),
      Ok(recordWithNoneField),
      (),
    )
  })

  test("Constructs unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->S.unsafeToUnknown
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

  test("Record constructing fails when constructor isn't provided", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ())

    t->Assert.deepEqual(
      recordStruct->S.construct(unknownRecord),
      Error("Struct missing constructor at root"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(recordStruct),
      Error("Struct missing constructor at root"),
      (),
    )
  })

  test("Nested record constructing fails when constructor isn't provided", t => {
    let record = {nested: {foo: "bar"}}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ()),
      ),
      ~constructor=nested => {nested: nested}->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.construct(unknownRecord),
      Error(`Struct missing constructor at ."nested"`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(recordStruct),
      Error(`Struct missing constructor at ."nested"`),
      (),
    )
  })

  test("Constructing fails when user returns error in a root record constructor", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~constructor=_ => Error("User error"),
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.construct(unknownRecord),
      Error("Struct constructing failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(recordStruct),
      Error("Struct constructing failed at root. Reason: User error"),
      (),
    )
  })

  test("Constructing fails when user returns error in a nested record constructor", t => {
    let record = {nested: {foo: "bar"}}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~constructor=_ => Error("User error"), ()),
      ),
      ~constructor=nested => {nested: nested}->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.construct(unknownRecord),
      Error(`Struct constructing failed at ."nested". Reason: User error`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.constructWith(recordStruct),
      Error(`Struct constructing failed at ."nested". Reason: User error`),
      (),
    )
  })

  test("Destructs unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ())

    t->Assert.deepEqual(recordStruct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(recordStruct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with multiple fields", t => {
    let record = {boo: "bar", zoo: "jee"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record2(
      ~fields=(("boo", S.string()), ("zoo", S.string())),
      ~destructor=({boo, zoo}) => (boo, zoo)->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(recordStruct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~destructor=({name, email, age}) => (name, email, age)->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.destruct(record), Ok(unknownRecord), ())
    t->Assert.deepEqual(record->S.destructWith(recordStruct), Ok(unknownRecord), ())
  })

  test("Destructs unknown record with optional nested record", t => {
    let recordWithSomeField = {singleFieldRecord: Some({foo: "bar"})}
    let recordWithNoneField = {singleFieldRecord: None}

    let unknownRecordWithSomeField =
      %raw(`{"singleFieldRecord":{"MUST_BE_MAPPED":"bar"}}`)->S.unsafeToUnknown
    let unknownRecordWithNoneField = %raw(`{"singleFieldRecord":undefined}`)->S.unsafeToUnknown

    let recordStruct = S.record1(
      ~fields=(
        "singleFieldRecord",
        S.option(
          S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~destructor=({foo}) => foo->Ok, ()),
        ),
      ),
      ~destructor=({singleFieldRecord}) => singleFieldRecord->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.destruct(recordWithSomeField),
      Ok(unknownRecordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      recordWithSomeField->S.destructWith(recordStruct),
      Ok(unknownRecordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      recordStruct->S.destruct(recordWithNoneField),
      Ok(unknownRecordWithNoneField),
      (),
    )
    t->Assert.deepEqual(
      recordWithNoneField->S.destructWith(recordStruct),
      Ok(unknownRecordWithNoneField),
      (),
    )
  })

  test("Destructs unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->S.unsafeToUnknown
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

  test("Record destructing fails when destructor isn't provided", t => {
    let record = {foo: "bar"}

    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~constructor=foo => {foo: foo}->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.destruct(record),
      Error("Struct missing destructor at root"),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(recordStruct),
      Error("Struct missing destructor at root"),
      (),
    )
  })

  test("Nested record destructing fails when destructor isn't provided", t => {
    let record = {nested: {foo: "bar"}}

    let recordStruct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~constructor=foo => {foo: foo}->Ok, ()),
      ),
      ~destructor=({nested}) => nested->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.destruct(record),
      Error(`Struct missing destructor at ."nested"`),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(recordStruct),
      Error(`Struct missing destructor at ."nested"`),
      (),
    )
  })

  test("Destructing fails when user returns error in a root record destructor", t => {
    let record = {foo: "bar"}

    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~destructor=_ => Error("User error"),
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.destruct(record),
      Error("Struct destructing failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(recordStruct),
      Error("Struct destructing failed at root. Reason: User error"),
      (),
    )
  })

  test("Destructing fails when user returns error in a nested record destructor", t => {
    let record = {nested: {foo: "bar"}}

    let recordStruct = S.record1(
      ~fields=(
        "nested",
        S.record1(~fields=("foo", S.string()), ~destructor=_ => Error("User error"), ()),
      ),
      ~destructor=({nested}) => nested->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.destruct(record),
      Error(`Struct destructing failed at ."nested". Reason: User error`),
      (),
    )
    t->Assert.deepEqual(
      record->S.destructWith(recordStruct),
      Error(`Struct destructing failed at ."nested". Reason: User error`),
      (),
    )
  })

  test("Constructs a record with fields mapping and destructs it back to the initial state", t => {
    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age}->Ok,
      ~destructor=({name, email, age}) => (name, email, age)->Ok,
      (),
    )

    t->Assert.deepEqual(
      recordStruct
      ->S.construct(unknownRecord)
      ->Belt.Result.map(record => recordStruct->S.destruct(record)),
      Ok(Ok(unknownRecord)),
      (),
    )
  })
}
