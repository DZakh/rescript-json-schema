type t

let make: S.t<'value> => t

let raw: (S.t<'value>, 'schema) => S.t<'value>
let description: (S.t<'value>, string) => S.t<'value>
