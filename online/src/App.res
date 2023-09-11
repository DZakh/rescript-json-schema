%%private(external magic: 'a => 'b = "%identity")

@module("json5") @scope("default")
external parseJson5: string => Js.Json.t = "parse"

// @module("@apidevtools/json-schema-ref-parser") @val
// external resolveRefs: JSONSchema.t => promise<JSONSchema.t> = "dereference"

@module("copy-to-clipboard")
external copy: string => unit = "default"

@react.component
let make = () => {
  let (json, setJson) = React.useState(() =>
    `{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "Id": { "type": "number" },
    "Title": { "type": "string" },
    "Tags": { "items": { "type": "string" }, "type": "array", "default": [] },
    "Rating": {
      "anyOf": [
        { "type": "string", "const": "G" },
        { "type": "string", "const": "PG" },
        { "type": "string", "const": "PG13" },
        { "type": "string", "const": "R" }
      ]
    },
    "Age": {
      "type": "integer",
      "deprecated": true,
      "description": "Use rating instead"
    }
  },
  "additionalProperties": true,
  "required": ["Id", "Title", "Rating"]
}`
  )
  let (inlinedStruct, setInlineStruct) = React.useState(() => "")
  let (errors, setErrors) = React.useState(() => "")

  React.useEffect1(() => {
    (
      async () => {
        try {
          let parsed = parseJson5(json)
          setErrors(_ => "")
          // TODO: Fix refs resolver in the browser environment
          // let schema = await parsed->(magic: Js.Json.t => JSONSchema.t)->resolveRefs
          let schema = parsed->(magic: Js.Json.t => JSONSchema.t)
          setInlineStruct(_ => schema->JSONSchema.toStruct->S.inline)->ignore
        } catch {
        | exn =>
          setErrors(_ =>
            `Errors:\n${exn
              ->Exn.asJsExn
              ->Option.flatMap(exn => exn->Exn.message)
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
          ->Option.flatMap(exn => exn->Exn.message)
          ->Option.getWithDefault("Unknown error")}`
      )
    }
  }

  <>
    <h1> {React.string("ReScript JSON Schema Online")} </h1>
    <div style={{display: "flex", justifyContent: "flex-grow"}}>
      <div
        style={
          display: "flex",
          flexDirection: "column",
          margin: "10px",
          padding: "10px",
          border: "1px solid grey",
          flexGrow: "1",
        }>
        <b> {React.string("Json Schema")} </b>
        <textarea
          style={width: "auto", height: "400px"}
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
          flexGrow: "1",
        }}>
        <b> {React.string("Result")} </b>
        <textarea
          style={
            width: "auto",
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
