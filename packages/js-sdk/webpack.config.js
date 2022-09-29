const path = require("path");
/* eslint-disable no-unused-vars */
const webpack = require("webpack");
/* eslint-enable no-unused-vars */
module.exports = {
    mode: "development",
    entry: "./src/index.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "index.js",
        library: "@superfluid-finance/js-sdk",
        libraryTarget: "umd",
        umdNamedDefine: true,
    },
    resolve: {
        fallback: {
            // Webpack 5, we must explicitly exclude @truffle/contract polyfills
            os: false,
            fs: false,
            url: false,
            assert: false,
            path: false,
            stream: false,
            https: false,
            http: false,
            crypto: false,
        },
    },
    externals: {
        "@truffle/contract": "@truffle/contract",
    },
};
