import { BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleConfigChanged,
    handleRewardAddressChanged,
    handleCFAv1LiquidationPeriodChanged,
    handlePPPConfigurationChanged,
    handleTrustedForwarderChanged,
} from "../../src/mappings/superfluidGovernance";
import { eventBasePropertyAssertion } from "../assertionHelper";
import { alice, bob, charlie, LIQUIDATION_PERIOD, PATRICIAN_PERIOD, TRUE } from "../constants";
import { keccak256String } from "../converters";
import {
    createConfigChangedEvent,
    createRewardAddressChangedEvent,
    createCFAv1LiquidationPeriodChangedEvent,
    createPPPConfigurationChangedEvent,
    createTrustedForwarderChangedEvent,
} from "./superfluidGovernance.helper";

describe("SuperfluidGovernance Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleConfigChanged() - Should create a new ConfigChangedEvent entity", () => {
            const host = alice;
            const superToken = bob;
            const key = keccak256String(
                "org.superfluid-finance.superfluid.trustedForwarder"
            );
            const isKeySet = true;
            const value = BigInt.fromI32(1);

            const ConfigChangedEvent = createConfigChangedEvent(
                host,
                superToken,
                key,
                isKeySet,
                value
            );

            handleConfigChanged(ConfigChangedEvent);

            const id = eventBasePropertyAssertion(
                ConfigChangedEvent,
                "ConfigChanged"
            );
            assert.fieldEquals("ConfigChangedEvent", id, "host", host);
            assert.fieldEquals("ConfigChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("ConfigChangedEvent", id, "key", key.toHexString());
            assert.fieldEquals("ConfigChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("ConfigChangedEvent", id, "value", value.toString());
        });

        test("handleRewardAddressChanged() - Should create a new RewardAddressChangedEvent entity", () => {
            const host = alice;
            const superToken = bob;
            const isKeySet = true;
            const rewardAddress = charlie;

            const RewardAddressChangedEvent = createRewardAddressChangedEvent(
                host,
                superToken,
                isKeySet,
                rewardAddress
            );

            handleRewardAddressChanged(RewardAddressChangedEvent);

            const id = eventBasePropertyAssertion(
                RewardAddressChangedEvent,
                "RewardAddressChanged"
            );
            assert.fieldEquals("RewardAddressChangedEvent", id, "host", host);
            assert.fieldEquals("RewardAddressChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("RewardAddressChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("RewardAddressChangedEvent", id, "rewardAddress", rewardAddress);
        });

        test("handleCFAv1LiquidationPeriodChanged() - Should create a new CFAv1LiquidationPeriodChangedEvent entity", () => {
            const host = alice;
            const superToken = bob;
            const isKeySet = true;

            const CFAv1LiquidationPeriodChangedEvent = createCFAv1LiquidationPeriodChangedEvent(
                host,
                superToken,
                isKeySet,
                LIQUIDATION_PERIOD
            );

            handleCFAv1LiquidationPeriodChanged(CFAv1LiquidationPeriodChangedEvent);

            const id = eventBasePropertyAssertion(
                CFAv1LiquidationPeriodChangedEvent,
                "CFAv1LiquidationPeriodChanged"
            );
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "host", host);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
        });

        test("handlePPPConfigurationChanged() - Should create a new PPPConfigurationChangedEvent entity", () => {
            const host = alice;
            const superToken = bob;
            const isKeySet = true;

            const PPPConfigurationChangedEvent = createPPPConfigurationChangedEvent(
                host,
                superToken,
                isKeySet,
                LIQUIDATION_PERIOD,
                PATRICIAN_PERIOD
            );

            handlePPPConfigurationChanged(PPPConfigurationChangedEvent);

            const id = eventBasePropertyAssertion(
                PPPConfigurationChangedEvent,
                "PPPConfigurationChanged"
            );
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "host", host);
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "patricianPeriod", PATRICIAN_PERIOD.toString());
        });

        test("handleTrustedForwarderChanged() - Should create a new TrustedForwarderChangedEvent entity", () => {
            const host = alice;
            const superToken = bob;
            const isKeySet = true;
            const forwarder = charlie;
            const enabled = true;

            const TrustedForwarderChangedEvent = createTrustedForwarderChangedEvent(
                host,
                superToken,
                isKeySet,
                forwarder,
                enabled
            );

            handleTrustedForwarderChanged(TrustedForwarderChangedEvent);

            const id = eventBasePropertyAssertion(
                TrustedForwarderChangedEvent,
                "TrustedForwarderChanged"
            );
            assert.fieldEquals("TrustedForwarderChangedEvent", id, "host", host);
            assert.fieldEquals("TrustedForwarderChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("TrustedForwarderChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("TrustedForwarderChangedEvent", id, "forwarder", forwarder);
            assert.fieldEquals("TrustedForwarderChangedEvent", id, "enabled", TRUE);
        });
    });
});
