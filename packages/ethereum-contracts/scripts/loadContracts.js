const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("../build/abi");
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
    // deploy-framework
    "TestResolver",
    "Superfluid",
    "SuperfluidMock",
    "SuperTokenFactory ",
    "SuperTokenFactoryMock ",
    "TestGovernance ",
    "ISuperfluidGovernance ",
    "Proxy",
    "Proxiable ",
    "ConstantFlowAgreementV1",
    "InstantDistributionAgreementV1"
];

const loadContracts = ({ isTruffle, web3Provider }) => {
    try {
        let contracts;
        if (!isTruffle) {
            console.debug("Using Superfluid scripts in an external or non-truffle environment");
            if (!web3Provider) throw new Error("web3Provider is required");
            // load contracts from ABI
            contractNames.forEach(name => {
                const c = (contracts[name] = TruffleContract({
                    contractName: name,
                    abi: SuperfluidABI[name]
                }));
                c.setProvider(web3Provider);
            });
        } else {
            console.debug("Using Superfluid scripts within the truffle environment");
            // load contracts from truffle artifacts
            contractNames.forEach(name => {
                contracts[name] = global.artifacts.require(name);
            });
        }
        return contracts;
    } catch (e) {
        throw Error(`@superfluid-finance/ethereum-contracts loadContracts(): ${e}`);
    }
};

module.exports = loadContracts;
