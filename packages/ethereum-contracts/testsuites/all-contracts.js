// Upgradability
require("../test/contracts/upgradability/UUPS.test.js");

// Libs
require("../test/contracts/libs/CallUtils.test.js");

// Superfluid
require("./superfluid-core.js");
require("./superfluid-governance.js");
require("./custom-tokens.js");

// App libraries
require("./apps-contracts.js");

// Scenarios
require("../test/contracts/scenarios/scenarios.js");

// Utility contracts
require("../test/contracts/utils/Resolver.test.js");
require("../test/contracts/utils/SuperUpgrader.test.js");
require("../test/contracts/utils/BatchLiquidator.test.js");
require("../test/contracts/utils/TOGA.test");

// Agreement forwarders
require("../test/contracts/agreements/CFAV1Forwarder.test.js");
