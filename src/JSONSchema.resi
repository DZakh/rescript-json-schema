@@uncurried

include module type of JSONSchema7

let make: S.t<'value> => result<t, string>

let extend: (S.t<'value>, t) => S.t<'value>

let toStruct: t => S.t<Js.Json.t>
