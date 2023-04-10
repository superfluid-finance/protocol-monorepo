// This file is used by solidity-coverage to configure the coverage report.

module.exports = {
    providerOptions: {
        network_id: 8555,
    },
    mocha: {
        grep: "@skip-on-coverage", // Find everything with this tag
        invert: true, // Run the grep's inverse set.
        enableTimeouts: false
    },
    configureYulOptimizer: true
};
