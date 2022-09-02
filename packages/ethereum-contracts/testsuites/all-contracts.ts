// Upgradability
require("../test/contracts/upgradability/UUPS.test.ts");

// Libs
require("../test/contracts/libs/CallUtils.test.ts");

// Superfluid
require("./superfluid-core.ts");
require("./superfluid-governance.ts");
require("./custom-tokens.ts");

// App libraries
require("./apps-contracts.ts");

// Scenarios
require("../test/contracts/scenarios/scenarios.js");

// Utility contracts
require("../test/contracts/utils/Resolver.test.js");
require("../test/contracts/utils/SuperUpgrader.test.js");
require("../test/contracts/utils/BatchLiquidator.test.js");
require("../test/contracts/utils/TOGA.test");

// Agreement forwarders
require("../test/contracts/agreements/CFAv1Forwarder.test.js");
