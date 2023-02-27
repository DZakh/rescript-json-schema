module Arrayable = JSONSchema7.Arrayable
type t = JSONSchema7.t
type definition = JSONSchema7.definition
type dependency = JSONSchema7.dependency
type typeName = JSONSchema7.typeName
type version = JSONSchema7.version
module Definition = JSONSchema7.Definition
module Dependency = JSONSchema7.Dependency

let make: S.t<'value> => result<t, string>

let extend: (S.t<'value>, t) => S.t<'value>

let description: (S.t<'value>, string) => S.t<'value>
