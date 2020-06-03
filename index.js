const TruffleContract = require("@truffle/contract");

module.exports = {
    load: (provider) => {
        let contracts = {
            TestResolver : TruffleContract(require("./build/contracts/FlowPayment.json")),
            FlowPayment : TruffleContract(require("./build/contracts/FlowPayment.json")),
            SuperToken : TruffleContract(require("./build/contracts/SuperToken.json")),
            IERC20 : TruffleContract(require("@openzeppelin/contracts/build/contracts/IERC20.json")),
        };
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    }
};
