const path = require("path");
const webpack = require("webpack");
module.exports = {
  mode: "development",
  entry: "./src/index.js",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "index.js",
    library: "@dappy/web3utils",
    libraryTarget: "umd",
    umdNamedDefine: true
  }
  // plugins: [new webpack.PrefetchPlugin('ethers')],
  // optimization: {
  //   splitChunks: {
  //     chunks: 'all'
  //   }
  // }
};
