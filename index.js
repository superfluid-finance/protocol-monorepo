const TruffleContract = require("@truffle/contract");

module.exports = {
    load: (provider) => {
        let contracts = {
            IERC20 : TruffleContract(require("@openzeppelin/contracts/build/contracts/IERC20.json")),
            TestResolver : TruffleContract(require("./build/contracts/TestResolver.json")),
            TestToken : TruffleContract(require("./build/contracts/TestToken.json")),
            IFlowAgreement : TruffleContract(require("./build/contracts/IFlowAgreement.json")),
            ISuperToken : TruffleContract(require("./build/contracts/ISuperToken.json")),
        };
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    }
};
