const contractNames = require("@superfluid-finance/ethereum-contracts/build/bundled-abi-contracts-list.json");
const abis = require("@superfluid-finance/ethereum-contracts/build/bundled-abi");

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
    if (web3) {
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
