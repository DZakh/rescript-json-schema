import { Schema, Json } from "rescript-schema";

export interface JSONSchema7 {}

export function make(schema: Schema<unknown>):
  | {
      TAG: "Ok";
      _0: JSONSchema7;
    }
  | {
      TAG: "Error";
      _0: string;
    };

export function toRescriptSchema(jsonSchema: JSONSchema7): Schema<Json>;

export function extend<Output, Input>(
  schema: Schema<Output, Input>,
  jsonSchema: JSONSchema7
): Schema<Output, Input>;

export function example<Output, Input>(
  schema: Schema<Output, Input>,
  example: Output
): Schema<Output, Input>;
