// Contract Deployment Scripts
const deployAuxContracts = require("./deploy-aux-contracts");
const deployDeterministically = require("./deploy-deterministically");
const deployFramework = require("./deploy-framework");

// Contract Deployment Scripts (Testing Only)
const deployErc1820 = require("./deploy-erc1820");
const deployTestEnvironment = require("./deploy-test-environment");
const deployTestFramework = require("./deploy-test-framework");

// SuperToken Deployment Scripts (Testing Only)
const deploySuperToken = require("./deploy-super-token");
const deployTestToken = require("./deploy-test-token");
const deployUnlistedPureSuperToken = require("./deploy-unlisted-pure-super-token");
const deployUnlistedSuperToken = require("./deploy-unlisted-super-token");

// Governance Actions Scripts
const govCreateNewAppRegistrationKey = require("./gov-create-new-app-registration-key");
const govCreateNewFactoryRegistration = require("./gov-create-new-factory-registration");
const govSet3PsConfig = require("./gov-set-3Ps-config");
const govSetRewardAddress = require("./gov-set-reward-address");
const govSetTokenMinDeposit = require("./gov-set-token-min-deposit");
const govSetTrustedForwarder = require("./gov-set-trusted-forwarder");
const govTransferFrameworkOwnership = require("./gov-transfer-framework-ownership");
const govUpgradeGovernance = require("./gov-upgrade-governance");
const govUpgradeSuperTokenLogic = require("./gov-upgrade-super-token-logic");

// Info Scripts
const infoInspectAccount = require("./info-inspect-account");
const infoListApps = require("./info-list-apps");
const infoPrintContractAddresses = require("./info-print-contract-addresses");
const infoScanDeployments = require("./info-scan-deployments");
const infoShowProtocol = require("./info-show-protocol");

// Resolver Actions Scripts
const resolverListSuperToken = require("./resolver-list-super-token");
const resolverRegisterToken = require("./resolver-register-token");
const resolverResetDeployment = require("./resolver-reset-deployment");
const resolverSetKeyValue = require("./resolver-set-key-value");
const resolverUnlistSuperToken = require("./resolver-unlist-super-token");

module.exports = {
    deployAuxContracts,
    deployDeterministically,
    deployErc1820,
    deployFramework,
    deploySuperToken,
    deployTestEnvironment,
    deployTestFramework,
    deployTestToken,
    deployUnlistedPureSuperToken,
    deployUnlistedSuperToken,
    govCreateNewAppRegistrationKey,
    govCreateNewFactoryRegistration,
    govSet3PsConfig,
    govSetRewardAddress,
    govSetTokenMinDeposit,
    govSetTrustedForwarder,
    govTransferFrameworkOwnership,
    govUpgradeGovernance,
    govUpgradeSuperTokenLogic,
    infoInspectAccount,
    infoListApps,
    infoPrintContractAddresses,
    infoScanDeployments,
    infoShowProtocol,
    resolverListSuperToken,
    resolverRegisterToken,
    resolverResetDeployment,
    resolverSetKeyValue,
    resolverUnlistSuperToken,
};
