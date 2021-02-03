const { TRUFFLE_NATIVE, TRUFFE_CONTRACT, ETHERS } = require("./constants");

const contractNames = require("../contracts.json");
const abis = require("../abi");

const mockContractNames = [
    "SuperfluidMock",
    "SuperTokenMockFactory",
    "SuperTokenFactoryMock"
];

const loadContracts = ({ mode, useMocks, web3Provider, from }) => {
    const allContractNames = [
        ...contractNames,
        ...(useMocks ? mockContractNames : [])
    ];
    try {
        let contracts = {};
        if (mode === ETHERS) {
            console.debug(
                "Using @superfluid-finance/js-sdk within the Ethers.js environment. Peer dependency @ethersproject/contract is required"
            );
            if (!web3Provider) throw new Error("web3Provider is required");
            const { Contract } = require("@ethersproject/contracts");
            allContractNames.forEach(name => {
                contracts[name] = {
                    at: address =>
                        new Contract(address, abis[name], web3Provider)
                };
            });
        } else if (mode === TRUFFLE_NATIVE) {
            try {
                console.debug(
                    "Using @superfluid-finance/js-sdk within a Truffle native environment. Truffle artifacts must be present."
                );
                allContractNames.forEach(name => {
                    contracts[name] = artifacts.require(name);
                });
            } catch (e) {
                throw Error(`could not load truffle artifacts. ${e}`);
            }
        } else {
            try {
                console.debug(
                    "Using @superfluid-finance/js-sdk in a non-native Truffle environment. Peer dependency @trufflesuite/contract is required."
                );
                if (!web3Provider) throw new Error("web3Provider is required");
                const TruffleContract = require("@truffle/contract");
                allContractNames.forEach(name => {
                    const c = (contracts[name] = TruffleContract({
                        contractName: name,
                        abi: abis[name]
                    }));
                    c.setProvider(web3Provider);
                    from && c.defaults({ from });
                });
            } catch (e) {
                throw Error(
                    `could not load non-truffle environment contracts. ${e}`
                );
            }
        }
        return contracts;
    } catch (e) {
        throw Error(`@superfluid-finance/js-sdk loadContracts(): ${e}`);
    }
};

module.exports = loadContracts;
