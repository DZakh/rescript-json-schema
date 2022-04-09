type t

let make: S.t<'value> => t

module Raw: {
  type t

  let make: 'rawSchema => t

  let string: t
  let integer: t
  let number: t
  let boolean: t
  let array: t => t
}

let raw: (S.t<'value>, Raw.t) => S.t<'value>
let description: (S.t<'value>, string) => S.t<'value>
