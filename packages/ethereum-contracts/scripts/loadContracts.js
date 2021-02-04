const path = require("path");

const contractNames = [
    "IERC20",
    "TokenInfo",
    "ERC20WithTokenInfo",
    "TestToken",
    "IResolver",
    "ISuperfluid",
    "ISuperToken",
    "ISuperTokenFactory",
    "ISuperAgreement",
    "IConstantFlowAgreementV1",
    "IInstantDistributionAgreementV1",
    "TestResolver",
    "Superfluid",
    "SuperTokenFactory",
    "TestGovernance",
    "ISuperfluidGovernance",
    "UUPSProxy",
    "UUPSProxiable",
    "ConstantFlowAgreementV1",
    "InstantDistributionAgreementV1",
    "ISETH",
    "SETHProxy"
];

const mockContractNames = [
    "SuperfluidMock",
    "SuperTokenMockFactory",
    "SuperTokenFactoryMock"
];

const getAdaptedContract = ({ address, abi, ethers }) => {
    const { Contract } = require("@ethersproject/contracts");

    const ethersContract = new Contract(
        address,
        abi,
        ethers.getSigner() || ethers
    );

    // Create adaptor for web3.js methods.encodeABI
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

const loadContracts = ({ ethers, web3, useMocks, from }) => {
    const allContractNames = [
        ...contractNames,
        ...(useMocks ? mockContractNames : [])
    ];
    const directoryPath = path.join(__dirname, "../build/contracts");
    try {
        let contracts = {};
        if (ethers) {
            try {
                console.debug(
                    `Using @superfluid-finance/ethereum-contracts within the Ethers.js environment.
                    Peer dependency @ethersproject/contract is required.`
                );
                allContractNames.forEach(name => {
                    const builtContract = require(path.join(
                        directoryPath,
                        name + ".json"
                    ));
                    contracts[name] = {
                        at: address =>
                            getAdaptedContract({
                                ethers,
                                address,
                                abi: builtContract.abi
                            }),
                        abi: builtContract.abi,
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
                    `Using @superfluid-finance/ethereum-contracts in a non-native Truffle environment.
                    Peer dependency @truffle/contract is required.`
                );
                const TruffleContract = require("@truffle/contract");
                allContractNames.forEach(name => {
                    const builtContract = require(path.join(
                        directoryPath,
                        name + ".json"
                    ));
                    const c = (contracts[name] = TruffleContract(
                        builtContract
                    ));
                    c.setProvider(web3.currentProvider);
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
                    `Using @superfluid-finance/ethereum-contracts within a Truffle native environment.
                    Truffle artifacts must be present.`
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
        throw Error(
            `@superfluid-finance/ethereum-contracts loadContracts(): ${e}`
        );
    }
};

module.exports = loadContracts;
