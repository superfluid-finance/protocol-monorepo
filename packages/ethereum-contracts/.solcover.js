module.exports = {
    providerOptions: {
        network_id: 6777,
    },
    skipFiles: [
        "mocks/",
        "apps",
        "libs/Strings.sol", // solidity test cases not included in coverage
    ],
    mocha: {
        grep: "@skip-on-coverage", // Find everything with this tag
        invert: true               // Run the grep's inverse set.
    }
};
