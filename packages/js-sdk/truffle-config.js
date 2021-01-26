/**
 * This truffle-config is only used for testing the js-sdk.
 *
 * See the @superfluid-finance/ethereum-contracts package for a good example.
 **/
const path = require("path");
module.exports = {
    contracts_build_directory: path.join(
        __dirname,
        "../ethereum-contracts/build/contracts"
    ),
    compilers: {
        solc: {
            version: "0.7.6"
        }
    }
};
