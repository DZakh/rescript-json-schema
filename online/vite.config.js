import { defineConfig } from "vite";
import rescript from "@jihchi/vite-plugin-rescript";

export default defineConfig({
  base: process.env.CI ? "/rescript-json-schema/" : "/",
  plugins: [rescript()],
});
