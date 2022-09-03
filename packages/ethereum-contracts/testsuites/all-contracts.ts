// Upgradability
import "../test/contracts/upgradability/UUPS.test";

// Libs
import "../test/contracts/libs/CallUtils.test";

// Superfluid
import "./superfluid-core";
import "./superfluid-governance";
import "./custom-tokens";

// App libraries
import "./apps-contracts";

// Scenarios
import "../test/contracts/scenarios/scenarios.test";

// Utility contracts
import "../test/contracts/utils/Resolver.test";
import "../test/contracts/utils/SuperUpgrader.test";
import "../test/contracts/utils/BatchLiquidator.test";
import "../test/contracts/utils/TOGA.test";

// Agreement forwarders
import "../test/contracts/agreements/CFAv1Forwarder.test";
