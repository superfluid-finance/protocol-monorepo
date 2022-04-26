// TODO: deprecate this, and move them to the right categories
require("../test/contracts/misc.test.js");

require("./superfluid-core.js");

require("./superfluid-governance.js");

require("./custom-tokens.js");

// apps helper contracts
require("./apps-contracts.js");

// scenarios
require("../test/scenarios/scenarios.js");

// Utility contracts
require("../test/contracts/utils/BatchLiquidator.test.js");
require("../test/contracts/utils/TOGA.test");

// UX contracts
require("../test/contracts/ux/SuperUpgrader.test.js");
