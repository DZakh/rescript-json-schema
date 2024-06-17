open Ava

test("Arrayable", t => {
  t->Assert.deepEqual(
    JSONSchema7.Arrayable.array([1, 2])->JSONSchema7.Arrayable.classify,
    JSONSchema7.Arrayable.Array([1, 2]),
  )
  t->Assert.deepEqual(
    JSONSchema7.Arrayable.single(1)->JSONSchema7.Arrayable.classify,
    JSONSchema7.Arrayable.Single(1),
  )
})

test("Definition", t => {
  t->Assert.deepEqual(
    JSONSchema7.Definition.schema({title: "foo"})->JSONSchema7.Definition.classify,
    JSONSchema7.Definition.Schema({title: "foo"}),
  )
  t->Assert.deepEqual(
    JSONSchema7.Definition.boolean(true)->JSONSchema7.Definition.classify,
    JSONSchema7.Definition.Boolean(true),
  )
})

test("Dependency", t => {
  t->Assert.deepEqual(
    JSONSchema7.Dependency.schema({title: "foo"})->JSONSchema7.Dependency.classify,
    JSONSchema7.Dependency.Schema({title: "foo"}),
  )
  t->Assert.deepEqual(
    JSONSchema7.Dependency.required(["field1", "field2"])->JSONSchema7.Dependency.classify,
    JSONSchema7.Dependency.Required(["field1", "field2"]),
  )
})
