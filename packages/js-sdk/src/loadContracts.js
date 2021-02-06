const contractNames = require("./contracts.json");
const abis = require("./abi");

const getAdaptedContract = ({ address, abi, ethers }) => {
    const { Contract } = require("@ethersproject/contracts");

    const ethersContract = new Contract(
        address,
        abi,
        ethers.getSigner() || ethers
    );

    // Create adapter for web3.js Contract.contract.methods.encodeABI
    const web3EncodingAdapter = {};
    ethersContract.interface.fragments.forEach(fragment => {
        web3EncodingAdapter[fragment.name] = (...args) => {
            return {
                encodeABI: () => {
                    return ethersContract.interface.encodeFunctionData(
                        fragment,
                        args
                    );
                }
            };
        };
    });
    ethersContract.contract = {
        methods: {
            ...web3EncodingAdapter
        }
    };

    return ethersContract;
};

const loadContracts = async ({
    isTruffle,
    ethers,
    web3,
    from,
    additionalContracts
}) => {
    const allContractNames = new Set([
        ...contractNames,
        ...(additionalContracts || [])
    ]);
    try {
        let contracts = {};
        if (ethers) {
            try {
                console.debug(
                    `Using @superfluid-finance/js-sdk within the Ethers.js environment.
                    Peer dependency @ethersproject/contract is required.`
                );
                allContractNames.forEach(name => {
                    contracts[name] = {
                        at: address =>
                            getAdaptedContract({
                                address,
                                ethers,
                                abi: abis[name]
                            }),
                        abi: abis[name],
                        contractName: name
                    };
                });
                if (from) {
                    throw new Error(
                        "Ethers mode does not support default from address"
                    );
                }
            } catch (e) {
                throw Error(
                    `could not load ethers environment contracts. ${e}`
                );
            }
        } else if (web3) {
            try {
                const TruffleContract = require("@truffle/contract");
                console.debug(
                    `Using @superfluid-finance/js-sdk in a non-native Truffle environment.
                    Peer dependency @truffle/contract is required.`
                );
                if (from) {
                    console.log("Set default from address to", from);
                } else {
                    const accounts = await web3.eth.getAccounts();
                    from = accounts[0];
                    console.log(
                        "Set default from address to the first account",
                        from
                    );
                }
                allContractNames.forEach(name => {
                    const c = (contracts[name] = TruffleContract({
                        contractName: name,
                        abi: abis[name]
                    }));
                    c.setProvider(web3.currentProvider);
                    c.defaults({ from });
                });
            } catch (e) {
                throw Error(
                    `could not load non-truffle environment contracts. ${e}`
                );
            }
        } else if (isTruffle) {
            try {
                console.debug(
                    `Using @superfluid-finance/js-sdk within a Truffle native environment.
                    Truffle artifacts must be present.`
                );
                if (from) {
                    console.log("Set Ddefault from address to", from);
                }
                allContractNames.forEach(name => {
                    const c = (contracts[name] = artifacts.require(name));
                    from && c.defaults({ from });
                });
            } catch (e) {
                throw Error(`could not load truffle artifacts. ${e}`);
            }
        } else {
            throw new Error("Unknown mode");
        }
        return contracts;
    } catch (e) {
        throw Error(`@superfluid-finance/js-sdk loadContracts(): ${e}`);
    }
};

module.exports = loadContracts;
