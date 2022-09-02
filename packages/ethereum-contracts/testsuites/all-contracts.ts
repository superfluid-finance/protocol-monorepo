// Upgradability
require("../test/contracts/upgradability/UUPS.test");

// Libs
require("../test/contracts/libs/CallUtils.test");

// Superfluid
require("./superfluid-core");
require("./superfluid-governance");
require("./custom-tokens");

// App libraries
require("./apps-contracts");

// Scenarios
require("../test/contracts/scenarios/scenarios");

// Utility contracts
require("../test/contracts/utils/Resolver.test");
require("../test/contracts/utils/SuperUpgrader.test");
require("../test/contracts/utils/BatchLiquidator.test");
require("../test/contracts/utils/TOGA.test");

// Agreement forwarders
require("../test/contracts/agreements/CFAv1Forwarder.test");
