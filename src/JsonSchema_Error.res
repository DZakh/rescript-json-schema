type locationComponent = Field(string)

type location = array<locationComponent>

type rec t = {kind: kind, mutable location: location}
and kind =
  | UnsupportedOptionalDictItem
  | UnsupportedOptionalArrayItem
  | UnsupportedNestedOptional
  | UnsupportedRootOptional
  | UnsupportedOptionalNullItem
  | UnsupportedOptionalTupleItem
  | UnsupportedOptionalUnionItem
  | UnsupportedStruct(string)
  | DefaultDestructingFailed({destructingErrorMessage: string})

module UnsupportedOptionalDictItem = {
  let make = () => {
    {kind: UnsupportedOptionalDictItem, location: []}
  }
}

module UnsupportedOptionalTupleItem = {
  let make = () => {
    {kind: UnsupportedOptionalTupleItem, location: []}
  }
}

module UnsupportedOptionalUnionItem = {
  let make = () => {
    {kind: UnsupportedOptionalUnionItem, location: []}
  }
}

module UnsupportedStruct = {
  let make = struct => {
    {kind: UnsupportedStruct(struct->S.name), location: []}
  }
}

module UnsupportedOptionalArrayItem = {
  let make = () => {
    {kind: UnsupportedOptionalArrayItem, location: []}
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
  let reason = switch error.kind {
  | UnsupportedOptionalDictItem => `Optional struct is not supported as Dict item`
  | UnsupportedOptionalArrayItem => `Optional struct is not supported as Array item`
  | UnsupportedRootOptional => `Optional struct is not supported at root`
  | UnsupportedOptionalNullItem => `Optional struct is not supported as Null item`
  | UnsupportedNestedOptional => `Optional struct is not supported inside the Option struct`
  | UnsupportedOptionalTupleItem => `Optional struct is not supported as Tuple item`
  | UnsupportedOptionalUnionItem => `Optional struct is not supported as Union item`
  | UnsupportedStruct(structName) => `The ${structName} struct is not supported`
  | DefaultDestructingFailed({destructingErrorMessage}) =>
    `Couldn't destruct default value. Error: ${destructingErrorMessage}`
  }
  `[ReScript JSON Schema] Failed converting at ${locationText}. Reason: ${reason}`
}
