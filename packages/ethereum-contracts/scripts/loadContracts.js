const fs = require("fs-extra");
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
    "SuperfluidMock",
    // TODO: Fix so the following can load via truffle artifacts
    "SuperTokenFactory ",
    "SuperTokenFactoryMock ",
    "TestGovernance ",
    "ISuperfluidGovernance ",
    "Proxy",
    "Proxiable ",
    "ConstantFlowAgreementV1",
    "InstantDistributionAgreementV1"
];

const loadContracts = ({ isTruffle, web3Provider, from }) => {
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
                const fileObjs = fs.readdirSync(directoryPath);
                fileObjs.forEach(fileName => {
                    const name = fileName.split(".")[0];
                    const builtContract = require(path.join(
                        directoryPath,
                        fileName
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
                contractNames.forEach(name => {
                    console.log(name);
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
