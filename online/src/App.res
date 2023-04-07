@module("json5") @scope("default")
external parseJson5: string => Js.Json.t = "parse"

// @module("@apidevtools/json-schema-ref-parser") @val
// external resolveRefs: JSONSchema.t => promise<JSONSchema.t> = "dereference"

@module("copy-to-clipboard")
external copy: string => unit = "default"

@react.component
let make = () => {
  let (json, setJson) = React.useState(() => "{}")
  let (inlinedStruct, setInlineStruct) = React.useState(() => "")
  let (errors, setErrors) = React.useState(() => "")

  React.useEffect1(() => {
    (
      async () => {
        try {
          let parsed = parseJson5(json)
          setErrors(_ => "")
          // TODO: Fix refs resolver in the browser environment
          // let schema = await parsed->(Obj.magic: Js.Json.t => JSONSchema.t)->resolveRefs
          let schema = parsed->(Obj.magic: Js.Json.t => JSONSchema.t)
          setInlineStruct(_ => schema->JSONSchema.toStruct->S.inline)->ignore
        } catch {
        | exn =>
          setErrors(_ =>
            `Errors:\n${exn
              ->Exn.asJsExn
              ->Option.flatMap(Exn.message)
              ->Option.getWithDefault("Unknown error")}`
          )
        }
      }
    )()->ignore

    None
  }, [json])

  let format = () => {
    try {
      setJson(_ => JSON.stringifyWithIndent(parseJson5(json), 2))
    } catch {
    | exn =>
      setErrors(_ =>
        `Errors:\n${exn
          ->Exn.asJsExn
          ->Option.flatMap(Exn.message)
          ->Option.getWithDefault("Unknown error")}`
      )
    }
  }

  <>
    <h1> {React.string("ReScript JSON Schema Online")} </h1>
    <div style={{display: "flex"}}>
      <div
        style={
          display: "flex",
          flexDirection: "column",
          margin: "10px",
          padding: "10px",
          border: "1px solid grey",
        }>
        <b> {React.string("Json Schema")} </b>
        <textarea
          style={width: "400px", height: "400px"}
          value={json}
          onChange={e => setJson((e->ReactEvent.Synthetic.target)["value"])}
        />
        <button style={{width: "100%"}} disabled={errors !== ""} onClick={_ => format()}>
          {React.string("Format")}
        </button>
      </div>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          margin: "10px",
          padding: "10px",
          border: "1px solid grey",
        }}>
        <b> {React.string("Result")} </b>
        <textarea
          style={
            width: "400px",
            height: "476px",
            color: errors === "" ? "black" : "red",
          }
          value={switch errors {
          | "" => inlinedStruct
          | _ => errors
          }}
          readOnly=true
        />
        <button style={{width: "100%"}} disabled={errors !== ""} onClick={_ => copy(inlinedStruct)}>
          {React.string("Copy")}
        </button>
      </div>
    </div>
    <a href="https://npmjs.com/package/rescript-json-schema">
      {React.string("Get the CLI NPM package here")}
    </a>
    <br />
    <a href="https://github.com/dzakh/rescript-json-schema">
      {React.string("Log an issue, open a feature PR or just leave a ⭐ here ^^")}
    </a>
  </>
}
