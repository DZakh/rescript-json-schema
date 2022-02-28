open Ava

test("Decodes unknown primitive", t => {
  let primitive = "ReScript is Great!"

  let unknownPrimitive = Js.Json.string(primitive)
  let primitiveStruct = S.string()

  t->Assert.is(primitiveStruct->S.decode(unknownPrimitive), primitive, ())
  t->Assert.is(unknownPrimitive->S.decodeWith(primitiveStruct), primitive, ())
})

test(
  "Decodes unknown primitive without validation. Note: Use Ajv.parse to safely decode with validation",
  t => {
    let primitive = 123.

    let unknownPrimitive = Js.Json.number(primitive)
    let primitiveStruct = S.string()

    t->Assert.is(primitiveStruct->S.decode(unknownPrimitive), primitive, ())
    t->Assert.is(unknownPrimitive->S.decodeWith(primitiveStruct), primitive, ())
  },
)

test("Decodes unknown array of primitives", t => {
  let arrayOfPrimitives = ["ReScript is Great!"]

  let unknownArrayOfPrimitives = Js.Json.stringArray(arrayOfPrimitives)
  let arrayOfPrimitivesStruct = S.array(S.string())

  t->Assert.deepEqual(
    arrayOfPrimitivesStruct->S.decode(unknownArrayOfPrimitives),
    arrayOfPrimitives,
    (),
  )
  t->Assert.deepEqual(
    unknownArrayOfPrimitives->S.decodeWith(arrayOfPrimitivesStruct),
    arrayOfPrimitives,
    (),
  )
})

module TestRecordDecoding = {
  type singleFieldRecord = {foo: string}
  type multipleFieldsRecord = {foo: string, zoo: string}
  type user = {name: string, email: string, age: int}
  type nestedRecord = {singleFieldRecord: singleFieldRecord}
  type optionalNestedRecord = {singleFieldRecord: option<singleFieldRecord>}

  test("Decodes unknown record with single field", t => {
    let record = {foo: "bar"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record1(~fields=("foo", S.string()), ~constructor=foo => {foo: foo})

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), record, ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), record, ())
  })

  test("Decodes unknown record with multiple fields", t => {
    let record = {foo: "bar", zoo: "jee"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record2(~fields=(("foo", S.string()), ("zoo", S.string())), ~constructor=((
      foo,
      zoo,
    )) => {foo: foo, zoo: zoo})

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), record, ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), record, ())
  })

  test("Decodes unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(("Name", S.string()), ("Email", S.string()), ("Age", S.int())),
      ~constructor=((name, email, age)) => {name: name, email: email, age: age},
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), record, ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), record, ())
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
        S.option(S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~constructor=foo => {foo: foo})),
      ),
      ~constructor=singleFieldRecord => {singleFieldRecord: singleFieldRecord},
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecordWithSomeField), recordWithSomeField, ())
    t->Assert.deepEqual(
      unknownRecordWithSomeField->S.decodeWith(recordStruct),
      recordWithSomeField,
      (),
    )
    t->Assert.deepEqual(recordStruct->S.decode(unknownRecordWithNoneField), recordWithNoneField, ())
    t->Assert.deepEqual(
      unknownRecordWithNoneField->S.decodeWith(recordStruct),
      recordWithNoneField,
      (),
    )
  })

  test("Decodes unknown array of records", t => {
    let arrayOfRecords = [{foo: "bar"}, {foo: "baz"}]

    let unknownArrayOfRecords =
      %raw(`[{"MUST_BE_MAPPED":"bar"},{"MUST_BE_MAPPED":"baz"}]`)->S.unsafeToUnknown
    let arrayOfRecordsStruct = S.array(
      S.record1(~fields=("MUST_BE_MAPPED", S.string()), ~constructor=foo => {foo: foo}),
    )

    t->Assert.deepEqual(arrayOfRecordsStruct->S.decode(unknownArrayOfRecords), arrayOfRecords, ())
    t->Assert.deepEqual(
      unknownArrayOfRecords->S.decodeWith(arrayOfRecordsStruct),
      arrayOfRecords,
      (),
    )
  })
}
