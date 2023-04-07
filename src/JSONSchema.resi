include module type of JSONSchema7

let make: S.t<'value> => result<t, string>

let extend: (S.t<'value>, t) => S.t<'value>

let toStruct: t => S.t<unknown>

let validate: (t, Js.Json.t) => result<unit, S.Error.t>
