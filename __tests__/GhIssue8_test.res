open Ava

%%private(external magic: 'a => 'b = "%identity")

test("Regression test", t => {
  let jsonSchema: JSONSchema.t = magic({
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "properties": {
      "page": {
        "type": "number",
        "minimum": 1,
        "default": 1,
      },
      "limit": {
        "type": "number",
        "minimum": 1,
        "default": 100,
      },
    },
    "additionalProperties": false,
  })

  t->Assert.deepEqual(
    jsonSchema->JSONSchema.toRescriptSchema->S.inline,
    `S.object(s =>
  {
    "page": s.field("page", S.option(S.float->S.Float.min(1., ~message="Number must be greater than or equal to 1"))->S.Option.getOr(%raw(\`1\`))),
    "limit": s.field("limit", S.option(S.float->S.Float.min(1., ~message="Number must be greater than or equal to 1"))->S.Option.getOr(%raw(\`100\`))),
  }
)->S.Object.strict`,
    (),
  )
})
