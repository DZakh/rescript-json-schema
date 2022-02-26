open Ava

test("Decodes unknown literal", t => {
  let literal = "ReScript is Great!"

  let unknownLiteral = Js.Json.string(literal)
  let literalStruct = S.string

  t->Assert.is(literalStruct->S.decode(unknownLiteral), literal, ())
  t->Assert.is(unknownLiteral->S.decodeWith(literalStruct), literal, ())
})

test(
  "Decodes unknown literal without validation. Note: Use Ajv.parse to safely decode with validation",
  t => {
    let literal = 123.

    let unknownLiteral = Js.Json.number(literal)
    let literalStruct = S.string

    t->Assert.is(literalStruct->S.decode(unknownLiteral), literal, ())
    t->Assert.is(unknownLiteral->S.decodeWith(literalStruct), literal, ())
  },
)

test("Decodes unknown array of literals", t => {
  let arrayOfLiterals = ["ReScript is Great!"]

  let unknownArrayOfLiterals = Js.Json.stringArray(arrayOfLiterals)
  let arrayOfLiteralsStruct = S.array(S.string)

  t->Assert.deepEqual(arrayOfLiteralsStruct->S.decode(unknownArrayOfLiterals), arrayOfLiterals, ())
  t->Assert.deepEqual(
    unknownArrayOfLiterals->S.decodeWith(arrayOfLiteralsStruct),
    arrayOfLiterals,
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
    let recordStruct = S.record1(~fields=S.field("foo", S.string), ~construct=foo => {foo: foo})

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), record, ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), record, ())
  })

  test("Decodes unknown record with multiple fields", t => {
    let record = {foo: "bar", zoo: "jee"}

    let unknownRecord = record->S.unsafeToUnknown
    let recordStruct = S.record2(
      ~fields=(S.field("foo", S.string), S.field("zoo", S.string)),
      ~construct=((foo, zoo)) => {foo: foo, zoo: zoo},
    )

    t->Assert.deepEqual(recordStruct->S.decode(unknownRecord), record, ())
    t->Assert.deepEqual(unknownRecord->S.decodeWith(recordStruct), record, ())
  })

  test("Decodes unknown record with mapped field", t => {
    let record = {name: "Dmitry", email: "dzakh.dev@gmail.com", age: 21}

    let unknownRecord =
      %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.unsafeToUnknown
    let recordStruct = S.record3(
      ~fields=(S.field("Name", S.string), S.field("Email", S.string), S.field("Age", S.int)),
      ~construct=((name, email, age)) => {name: name, email: email, age: age},
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
      ~fields=S.field(
        "singleFieldRecord",
        S.option(
          S.record1(~fields=S.field("MUST_BE_MAPPED", S.string), ~construct=foo => {foo: foo}),
        ),
      ),
      ~construct=singleFieldRecord => {singleFieldRecord: singleFieldRecord},
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
      S.record1(~fields=S.field("MUST_BE_MAPPED", S.string), ~construct=foo => {foo: foo}),
    )

    t->Assert.deepEqual(arrayOfRecordsStruct->S.decode(unknownArrayOfRecords), arrayOfRecords, ())
    t->Assert.deepEqual(
      unknownArrayOfRecords->S.decodeWith(arrayOfRecordsStruct),
      arrayOfRecords,
      (),
    )
  })
}
