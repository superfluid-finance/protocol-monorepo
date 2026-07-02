/**
 * Hardhat runner for legacy truffle-style ops scripts (colon argv via process.argv).
 */
async function main() {
    const script = require("./info-print-contract-addresses");
    await new Promise((resolve, reject) => {
        script((err) => (err ? reject(err) : resolve()));
    });
}

module.exports = main;
