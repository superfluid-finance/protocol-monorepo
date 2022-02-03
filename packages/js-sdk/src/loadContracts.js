const contractNames = require("./contracts.json");
const abis = require("./abi");

const getAdaptedContract = ({address, abi, ethers}) => {
    const {Contract} = require("@ethersproject/contracts");

    let providerOrSigner = ethers;
    try {
        providerOrSigner = ethers.getSigner();
    } catch (e) {
        console.debug(
            "Ethers.js signer is not available. Using read-only mode."
        );
    }
    const ethersContract = new Contract(address, abi, providerOrSigner);

    // Create adapter for web3.js Contract.contract.methods.encodeABI
    const web3EncodingAdapter = {};
    ethersContract.interface.fragments.forEach((fragment) => {
        web3EncodingAdapter[fragment.name] = (...args) => {
            return {
                encodeABI: () => {
                    return ethersContract.interface.encodeFunctionData(
                        fragment,
                        args
                    );
                },
            };
        };
    });
    ethersContract.contract = {
        methods: {
            ...web3EncodingAdapter,
        },
    };
    ethersContract.abi = abi;

    return ethersContract;
};

function defaultContractLoader(name) {
    if (name in abis) {
        return {
            contractName: name,
            abi: abis[name],
        };
    } else throw Error(`Cannot load contract "${name}"`);
}

function setTruffleContractDefaults(c, {networkId, from}) {
    c.autoGas = true;
    c.estimateGas = 1.25;
    c.setNetwork(networkId);
    const defaults = {};
    from && (defaults.from = from);
    c.defaults(defaults);
}

const loadContracts = async ({
    isTruffle,
    ethers,
    web3,
    from,
    additionalContracts,
    contractLoader,
    networkId,
}) => {
    if (!networkId) throw Error("networkId not provided");
    // use set to eliminate duplicated entries
    const allContractNames = Array.from(
        new Set([...contractNames, ...(additionalContracts || [])])
    );
    contractLoader = contractLoader || defaultContractLoader;
    let contracts = {};
    if (ethers) {
        try {
            console.debug(
                `Using @superfluid-finance/js-sdk within the Ethers.js environment.
                Peer dependency @ethersproject/contract is required.`
            );
            await Promise.all(
                allContractNames.map(async (name) => {
                    const contract = await contractLoader(name);
                    contracts[name] = {
                        at: (address) =>
                            getAdaptedContract({
                                address,
                                ethers,
                                abi: contract.abi,
                            }),
                        abi: contract.abi,
                        contractName: name,
                    };
                })
            );
            if (from) {
                throw Error(
                    "Ethers mode does not support default from address"
                );
            }
        } catch (e) {
            throw Error(`could not load ethers environment contracts. ${e}`);
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
            await Promise.all(
                allContractNames.map(async (name) => {
                    const _normalizedObject = await contractLoader(name);
                    Object.assign(_normalizedObject, {
                        networks: {
                            [networkId]: {},
                            // setting it for truffle contract detectNetwork method
                        },
                    });
                    const c = (contracts[name] =
                        TruffleContract(_normalizedObject));
                    c.setProvider(web3.currentProvider);
                    setTruffleContractDefaults(c, {
                        networkId,
                        from,
                    });
                })
            );
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
                console.log("Set default from address to", from);
            }
            allContractNames.forEach((name) => {
                const c = (contracts[name] = artifacts.require(name));
                setTruffleContractDefaults(c, {
                    networkId,
                    from,
                });
            });
        } catch (e) {
            throw Error(`could not load truffle artifacts. ${e}`);
        }
    } else {
        throw Error("Unknown mode");
    }
    return contracts;
};

module.exports = loadContracts;
