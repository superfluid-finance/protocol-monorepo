module.exports = {
    providerOptions: {
        network_id: 6777,
    },
    skipFiles: [
        "mocks/",
        "apps",
        "utils/Strings.sol", // solidity test cases not included in coverage
    ]
};
