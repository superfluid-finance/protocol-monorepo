// This file is used by solidity-coverage to configure the coverage report.

module.exports = {
    providerOptions: {
        network_id: 8555,
    },
    skipFiles: [
        "mocks/",
        // we skip the coverage for the SuperAppBase contracts because
        // we override the functions in child contracts
        "apps/SuperAppBase.sol",
        "apps/SuperAppBaseFlow.sol",
        "apps/SuperfluidLoaderLibrary.sol",

        // we skip the coverage for these contracts because they are
        // only used for testing
        "utils/SuperfluidFrameworkDeployer",
        "utils/SuperfluidFrameworkDeploymentSteps",
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
