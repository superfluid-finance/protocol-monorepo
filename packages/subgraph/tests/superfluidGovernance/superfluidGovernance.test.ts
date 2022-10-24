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
import { assertEventBaseProperties } from "../assertionHelper";
import {
    charlie,
    hostAddress,
    LIQUIDATION_PERIOD,
    maticx,
    PATRICIAN_PERIOD,
    TRUE,
} from "../constants";
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
            const host = hostAddress;
            const superToken = maticx;
            const key = keccak256String(
                "org.superfluid-finance.superfluid.trustedForwarder"
            );
            const isKeySet = true;
            const value = BigInt.fromI32(1);

            const configChangedEvent = createConfigChangedEvent(
                host,
                superToken,
                key,
                isKeySet,
                value
            );

            handleConfigChanged(configChangedEvent);

            const id = assertEventBaseProperties(
                configChangedEvent,
                "ConfigChanged"
            );
            assert.fieldEquals("ConfigChangedEvent", id, "host", host);
            assert.fieldEquals("ConfigChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("ConfigChangedEvent", id, "key", key.toHexString());
            assert.fieldEquals("ConfigChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("ConfigChangedEvent", id, "value", value.toString());
        });

        test("handleRewardAddressChanged() - Should create a new RewardAddressChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticx;
            const isKeySet = true;
            const rewardAddress = charlie;

            const rewardAddressChangedEvent = createRewardAddressChangedEvent(
                host,
                superToken,
                isKeySet,
                rewardAddress
            );

            handleRewardAddressChanged(rewardAddressChangedEvent);

            const id = assertEventBaseProperties(
                rewardAddressChangedEvent,
                "RewardAddressChanged"
            );
            assert.fieldEquals("RewardAddressChangedEvent", id, "host", host);
            assert.fieldEquals("RewardAddressChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("RewardAddressChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("RewardAddressChangedEvent", id, "rewardAddress", rewardAddress);
        });

        test("handleCFAv1LiquidationPeriodChanged() - Should create a new CFAv1LiquidationPeriodChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticx;
            const isKeySet = true;

            const cfaV1LiquidationPeriodChangedEvent = createCFAv1LiquidationPeriodChangedEvent(
                host,
                superToken,
                isKeySet,
                LIQUIDATION_PERIOD
            );

            handleCFAv1LiquidationPeriodChanged(cfaV1LiquidationPeriodChangedEvent);

            const id = assertEventBaseProperties(
                cfaV1LiquidationPeriodChangedEvent,
                "CFAv1LiquidationPeriodChanged"
            );
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "host", host);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("CFAv1LiquidationPeriodChangedEvent", id, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
        });

        test("handlePPPConfigurationChanged() - Should create a new PPPConfigurationChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticx;
            const isKeySet = true;

            const pppConfigurationChangedEvent = createPPPConfigurationChangedEvent(
                host,
                superToken,
                isKeySet,
                LIQUIDATION_PERIOD,
                PATRICIAN_PERIOD
            );

            handlePPPConfigurationChanged(pppConfigurationChangedEvent);

            const id = assertEventBaseProperties(
                pppConfigurationChangedEvent,
                "PPPConfigurationChanged"
            );
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "host", host);
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
            assert.fieldEquals("PPPConfigurationChangedEvent", id, "patricianPeriod", PATRICIAN_PERIOD.toString());
        });

        test("handleTrustedForwarderChanged() - Should create a new TrustedForwarderChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticx;
            const isKeySet = true;
            const forwarder = charlie;
            const enabled = true;

            const trustedForwarderChangedEvent = createTrustedForwarderChangedEvent(
                host,
                superToken,
                isKeySet,
                forwarder,
                enabled
            );

            handleTrustedForwarderChanged(trustedForwarderChangedEvent);

            const id = assertEventBaseProperties(
                trustedForwarderChangedEvent,
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
