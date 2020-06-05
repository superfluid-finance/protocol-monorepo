const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("./build/abi");

module.exports = {
    load: (provider) => {
        let contracts = {};
        Object.keys(SuperfluidABI).forEach(i => {
            contracts[i] = TruffleContract({
                contractName: i,
                abi: SuperfluidABI[i]
            });
        });
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    }
};
