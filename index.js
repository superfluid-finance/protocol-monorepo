const TruffleContract = require("@truffle/contract");

module.exports = {
    load: (provider) => {
        let contracts = {
            SimpleVault : TruffleContract(require("./build/contracts/SimpleVault.json")),
            IERC20 : TruffleContract(require("@openzeppelin/contracts/build/contracts/IERC20.json")),
        };
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    }
};
