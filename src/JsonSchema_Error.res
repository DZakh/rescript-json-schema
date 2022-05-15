type locationComponent = Field(string)

type location = array<locationComponent>

type rec t = {kind: kind, mutable location: location}
and kind =
  | UnsupportedOptionalDictItem
  | UnsupportedOptionalArrayItem
  | UnsupportedNestedOptional
  | UnsupportedRootOptional
  | UnsupportedOptionalNullItem
  | UnsupportedEmptyOptionLiteral
  | DefaultDestructingFailed({destructingErrorMessage: string})

module UnsupportedOptionalDictItem = {
  let make = () => {
    {kind: UnsupportedOptionalDictItem, location: []}
  }
}

module UnsupportedOptionalArrayItem = {
  let make = () => {
    {kind: UnsupportedOptionalArrayItem, location: []}
  }
}

module UnsupportedEmptyOptionLiteral = {
  let make = () => {
    {kind: UnsupportedEmptyOptionLiteral, location: []}
  }
}

module UnsupportedOptionalNullItem = {
  let make = () => {
    {kind: UnsupportedOptionalNullItem, location: []}
  }
}

module UnsupportedNestedOptional = {
  let make = () => {
    {kind: UnsupportedNestedOptional, location: []}
  }
}

module UnsupportedRootOptional = {
  let make = () => {
    {kind: UnsupportedRootOptional, location: []}
  }
}

module DefaultDestructingFailed = {
  let make = (~destructingErrorMessage) => {
    {
      kind: DefaultDestructingFailed({destructingErrorMessage: destructingErrorMessage}),
      location: [],
    }
  }
}

let formatLocation = location => {
  if location->Js.Array2.length === 0 {
    "root"
  } else {
    location
    ->Js.Array2.map(s =>
      switch s {
      | Field(field) => `["` ++ field ++ `"]`
      }
    )
    ->Js.Array2.joinWith("")
  }
}

let prependField = (error, field) => {
  error.location = [Field(field)]->Js.Array2.concat(error.location)
  error
}

let toString = error => {
  let locationText = error.location->formatLocation
  let message = switch error.kind {
  | UnsupportedOptionalDictItem =>
    `Failed converting at ${locationText}. Reason: Optional struct is not supported as Dict item yet`
  | UnsupportedOptionalArrayItem =>
    `Failed converting at ${locationText}. Reason: Optional struct is not supported as Array item yet`
  | UnsupportedRootOptional =>
    `Failed converting at ${locationText}. Reason: Optional struct is not supported at root yet`
  | UnsupportedEmptyOptionLiteral =>
    `Failed converting at ${locationText}. Reason: The EmptyOption struct is not supported yet`
  | UnsupportedOptionalNullItem =>
    `Failed converting at ${locationText}. Reason: Optional struct is not supported as Null item yet`
  | UnsupportedNestedOptional =>
    `Failed converting at ${locationText}. Reason: Optional struct is not supported inside the Option struct yet`
  | DefaultDestructingFailed({destructingErrorMessage}) =>
    `Failed converting at ${locationText}. Reason: Couldn't destruct default value. Error: ${destructingErrorMessage}`
  }
  `[ReScript JSON Schema] ${message}`
}
