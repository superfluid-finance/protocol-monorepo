const path = require("path");

const TruffleContract = require("@truffle/contract");

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
    "InstantDistributionAgreementV1"
];

const mockContractNames = ["SuperfluidMock", "SuperTokenFactoryMock"];

const loadContracts = ({ isTruffle, useMocks, web3Provider, from }) => {
    const allContractNames = [
        ...contractNames,
        ...(useMocks ? mockContractNames : [])
    ];
    try {
        let contracts = {};
        if (!isTruffle) {
            // if (true) {
            try {
                console.debug(
                    "Using Superfluid scripts in an external or non-truffle environment"
                );
                if (!web3Provider) throw new Error("web3Provider is required");
                // load contracts from ABI
                const directoryPath = path.join(
                    __dirname,
                    "../build/contracts"
                );
                allContractNames.forEach(name => {
                    const builtContract = require(path.join(
                        directoryPath,
                        name + ".json"
                    ));
                    const c = (contracts[name] = TruffleContract(
                        builtContract,
                        name
                    ));
                    c.setProvider(web3Provider);
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
                    "Using Superfluid scripts within the truffle environment"
                );
                // load contracts from truffle artifacts
                allContractNames.forEach(name => {
                    //console.log(name);
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
