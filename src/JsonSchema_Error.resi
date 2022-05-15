type t

module UnsupportedOptionalDictItem: {
  let make: unit => t
}

module UnsupportedOptionalArrayItem: {
  let make: unit => t
}

module UnsupportedNestedOptional: {
  let make: unit => t
}

module UnsupportedOptionalNullItem: {
  let make: unit => t
}

module UnsupportedEmptyOptionLiteral: {
  let make: unit => t
}

module UnsupportedRootOptional: {
  let make: unit => t
}

module DefaultDestructingFailed: {
  let make: (~destructingErrorMessage: string) => t
}

let prependField: (t, string) => t

let toString: t => string
