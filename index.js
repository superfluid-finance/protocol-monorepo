const TruffleContract = require("@truffle/contract");

module.exports = {
    load: (provider) => {
        let contracts = {
            FlowPayment : TruffleContract(require("./build/contracts/FlowPayment.json")),
            IERC20 : TruffleContract(require("@openzeppelin/contracts/build/contracts/IERC20.json")),
        };
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    }
};
