import typescript from "@rollup/plugin-typescript";

export default {
    input: "src/index.ts",
    output: [
        {
            file: "index.js",
            format: "cjs",
        },
        {
			name: "cdn-js",
            file: "core-sdk.js",
            format: "umd",
        },
    ],
    plugins: [typescript()],
};
