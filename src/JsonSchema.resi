type t

let make: S.t<'value> => result<t, string>

let raw: (S.t<'value>, 'raw) => S.t<'value>

let description: (S.t<'value>, string) => S.t<'value>
