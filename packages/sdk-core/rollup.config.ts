import typescript from "@rollup/plugin-typescript";
import json from "@rollup/plugin-json";

const cjsConfig = {
    input: "src/index.ts",
    output: [
        {
            file: "dist/main/index.js",
            format: "cjs",
        },
    ],
    plugins: [typescript({ tsconfig: "./tsconfig.json" }), json()],
};

const esConfig = {
    input: "src/index.ts",
    output: [
        {
            file: "dist/module/index.js",
            format: "es",
        },
    ],
    plugins: [typescript(), json()],
};

export default [cjsConfig, esConfig];
