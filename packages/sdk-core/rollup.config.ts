import typescript from "@rollup/plugin-typescript";

export default {
    input: "src/index.ts",
    output: [
        {
            file: "dist/main/index.js",
            format: "cjs",
        },
        {
            file: "dist/module/index.js",
            format: "es",
        },
        {
            name: "cdn-js",
            file: "dist/umd/index.js",
            format: "umd",
        },
    ],
	plugins: [typescript({ tsconfig: "./tsconfig.json" })],
};
