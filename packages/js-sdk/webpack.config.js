const path = require("path");
const webpack = require("webpack");
module.exports = {
    mode: "development",
    entry: "./src/index.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "index.js",
        library: "@superfluid-finance/js-sdk",
        libraryTarget: "umd",
        umdNamedDefine: true
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                loader: "babel-loader"
            }
        ]
    },
    resolve: {
        fallback: {
            // Webpack 5, we must explicitly exclude @truffle/contract polyfills
            os: false,
            url: false,
            assert: false,
            path: false,
            stream: false,
            https: false,
            http: false,
            crypto: false
        }
    }
};
