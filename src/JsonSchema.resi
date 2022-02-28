type t<'value>

let make: S.t<'value> => t<'value>

let raw: (S.t<'value>, 'schema) => S.t<'value>
let description: (S.t<'value>, string) => S.t<'value>
