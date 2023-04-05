module.exports = {
    providerOptions: {
        network_id: 8555,
    },
    skipFiles: [
        "mocks/",
        "apps/SuperAppBase.sol",
        "utils/SuperfluidFrameworkDeployer",
        "utils/SuperTokenDeployer",
        "utils/TestToken",
        "utils/TestResolver",
        "utils/TestGovernance",
    ],
    mocha: {
        grep: "@skip-on-coverage", // Find everything with this tag
        invert: true, // Run the grep's inverse set.
        enableTimeouts: false
    },
    configureYulOptimizer: true
};
