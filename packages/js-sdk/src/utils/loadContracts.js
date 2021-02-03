const { TRUFFLE_NATIVE, ETHERS } = require("./constants");

const contractNames = require("../contracts.json");
const abis = require("../abi");

const mockContractNames = [
    "SuperfluidMock",
    "SuperTokenMockFactory",
    "SuperTokenFactoryMock"
];

const loadContracts = ({ ethers, web3, useMocks, web3Provider, from }) => {
    const allContractNames = [
        ...contractNames,
        ...(useMocks ? mockContractNames : [])
    ];
    try {
        let contracts = {};
        if (ethers) {
            try {
                console.debug(
                    "Using @superfluid-finance/js-sdk within the Ethers.js environment. Peer dependency @ethersproject/contract is required."
                );
                const { Contract } = require("@ethersproject/contracts");
                allContractNames.forEach(name => {
                    contracts[name] = {
                        at: address => {
                            const ethersContract = new Contract(
                                address,
                                abis[name],
                                ethers
                            );
                            const web3EncodingAdapter = {};
                            ethersContract.interface.fragments.forEach(
                                fragment => {
                                    web3EncodingAdapter[fragment.name] = (
                                        ...arguments
                                    ) => {
                                        return {
                                            encodeABI: () => {
                                                return ethersContract.interface.encodeFunctionData(
                                                    fragment,
                                                    arguments
                                                );
                                            }
                                        };
                                    };
                                }
                            );
                            ethersContract.contract = {
                                methods: {
                                    ...web3EncodingAdapter
                                }
                            };
                            return ethersContract;
                        },
                        abi: abis[name],
                        contractName: name
                    };
                });
            } catch (e) {
                throw Error(
                    `could not load ethers environment contracts. ${e}`
                );
            }
        } else if (web3) {
            try {
                console.debug(
                    "Using @superfluid-finance/js-sdk in a non-native Truffle environment. Peer dependency @truffle/contract is required."
                );
                const TruffleContract = require("@truffle/contract");
                allContractNames.forEach(name => {
                    const c = (contracts[name] = TruffleContract({
                        contractName: name,
                        abi: abis[name]
                    }));
                    c.setProvider(web3);
                    from && c.defaults({ from });
                });
            } catch (e) {
                throw Error(
                    `could not load non-truffle environment contracts. ${e}`
                );
            }
        } else {
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
        }
        return contracts;
    } catch (e) {
        throw Error(`@superfluid-finance/js-sdk loadContracts(): ${e}`);
    }
};

module.exports = loadContracts;
