open Ava
type throwsExpectation = {message: option<string>}

test("Decodes unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let primitiveStruct = S.string()

  t->Assert.deepEqual(primitiveStruct->S.decode(unknownPrimitive), Ok(primitive), ())
  t->Assert.deepEqual(unknownPrimitive->S.decodeWith(primitiveStruct), Ok(primitive), ())
})

test(
  "Decodes unknown primitive without validation. Note: Use Ajv.parse to safely decode with validation",
  t => {
    let primitive = 123.

    let unknownPrimitive = Js.Json.number(primitive)
    let primitiveStruct = S.string()

    t->Assert.deepEqual(primitiveStruct->S.decode(unknownPrimitive), Ok(primitive), ())
    t->Assert.deepEqual(unknownPrimitive->S.decodeWith(primitiveStruct), Ok(primitive), ())
  },
)

test("Decodes unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let arrayOfPrimitivesStruct = S.array(S.string())

  t->Assert.deepEqual(
    arrayOfPrimitivesStruct->S.decode(unknownArrayOfPrimitives),
    Ok(arrayOfPrimitives),
    (),
  )
  t->Assert.deepEqual(
    unknownArrayOfPrimitives->S.decodeWith(arrayOfPrimitivesStruct),
    Ok(arrayOfPrimitives),
    (),
  )
})

module TestRecordDecoding = {
  type singleFieldRecord = {foo: string}
  type multipleFieldsRecord = {boo: string, zoo: string}
  type user = {name: string, email: string, age: int}
  type nestedRecord = {nested: singleFieldRecord}
  type optionalNestedRecord = {singleFieldRecord: option<singleFieldRecord>}

  test("Decodes unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~constructor=foo => {foo: foo}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), Ok(record), ())
  })

  test("Decodes unknown record with multiple fields", t => {
    let record = {boo: "bar", zoo: "jee"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record2(
      ~fields=(("boo", S.string()), ("zoo", S.string())),
      ~constructor=((boo, zoo)) => {boo: boo, zoo: zoo}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), Ok(record), ())
  })

  test("Decodes unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age}->Ok,
      (),
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), Ok(record), ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), Ok(record), ())
  })

  test("Decodes unknown record with optional nested record", t => {
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
      recordStruct->S.decode(unknownRecordWithSomeField),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithSomeField->S.decodeWith(recordStruct),
      Ok(recordWithSomeField),
      (),
    )
    t->Assert.deepEqual(
      recordStruct->S.decode(unknownRecordWithNoneField),
      Ok(recordWithNoneField),
      (),
    )
    t->Assert.deepEqual(
      unknownRecordWithNoneField->S.decodeWith(recordStruct),
      Ok(recordWithNoneField),
      (),
    )
  })

  test("Decodes unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->S.unsafeToUnknown
    let arrayOfRecordsStruct = S.array(
      S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~constructor=foo => {foo: foo}->Ok, ()),
    )

    t->Assert.deepEqual(
      arrayOfRecordsStruct->S.decode(unknownArrayOfRecords),
      Ok(arrayOfRecords),
      (),
    )
    t->Assert.deepEqual(
      unknownArrayOfRecords->S.decodeWith(arrayOfRecordsStruct),
      Ok(arrayOfRecords),
      (),
    )
  })

  test("Throws for a Record factory without either a constructor, or a destructor", t => {
    t->Assert.throws(
      () => {
        S.record1(~fields=("any", S.string()), ())->ignore
      },
      ~expectations={
        message: Some("For a Record struct either a constructor, or a destructor is required"),
      },
      (),
    )
  })

  test("Record decoding fails when constructor isn't provided", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(~fields=("foo", S.string()), ~destructor=({foo}) => foo->Ok, ())

    t->Assert.deepEqual(
      recordStruct->S.decode(unknownRecord),
      Error("Struct missing decoder at root"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.decodeWith(recordStruct),
      Error("Struct missing decoder at root"),
      (),
    )
  })

  test("Nested record decoding fails when constructor isn't provided", t => {
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
      recordStruct->S.decode(unknownRecord),
      Error(`Struct missing decoder at ."nested"`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.decodeWith(recordStruct),
      Error(`Struct missing decoder at ."nested"`),
      (),
    )
  })

  test("Decoding fails when user returns error in a root record constructor", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(
      ~fields=("foo", S.string()),
      ~constructor=_ => Error("User error"),
      (),
    )

    t->Assert.deepEqual(
      recordStruct->S.decode(unknownRecord),
      Error("Struct decoding failed at root. Reason: User error"),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.decodeWith(recordStruct),
      Error("Struct decoding failed at root. Reason: User error"),
      (),
    )
  })

  test("Decoding fails when user returns error in a nested record constructor", t => {
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
      recordStruct->S.decode(unknownRecord),
      Error(`Struct decoding failed at ."nested". Reason: User error`),
      (),
    )
    t->Assert.deepEqual(
      unknownRecord->S.decodeWith(recordStruct),
      Error(`Struct decoding failed at ."nested". Reason: User error`),
      (),
    )
  })
}
