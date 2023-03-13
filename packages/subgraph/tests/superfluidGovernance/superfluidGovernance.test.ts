import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    newMockEvent,
    test,
} from "matchstick-as/assembly/index";
import {
    handleConfigChanged,
    handleRewardAddressChanged,
    handleCFAv1LiquidationPeriodChanged,
    handlePPPConfigurationChanged,
    handleTrustedForwarderChanged,
    handleSuperTokenMinimumDepositChanged,
} from "../../src/mappings/superfluidGovernance";
import { assertEventBaseProperties } from "../assertionHelpers";
import {
    charlie,
    DEFAULT_DECIMALS,
    hostAddress,
    LIQUIDATION_PERIOD,
    maticXAddress,
    maticXName,
    maticXSymbol,
    PATRICIAN_PERIOD,
    TRUE,
} from "../constants";
import { keccak256String } from "../converters";
import { createSuperToken } from "../mockedEntities";
import {
    createConfigChangedEvent,
    createRewardAddressChangedEvent,
    createCFAv1LiquidationPeriodChangedEvent,
    createPPPConfigurationChangedEvent,
    createTrustedForwarderChangedEvent,
    createSuperTokenMinimumDepositChangedEvent,
} from "./superfluidGovernance.helper";

const STRING_ZERO_ADDRESS = Address.zero().toHexString();

describe("SuperfluidGovernance Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleConfigChanged() - Should create ConfigChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
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

        test("handleRewardAddressChanged() - Should create RewardAddressChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
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

        test("handleCFAv1LiquidationPeriodChanged() - Should create CFAv1LiquidationPeriodChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
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

        test("handlePPPConfigurationChanged() - Should create PPPConfigurationChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
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

        test("handleTrustedForwarderChanged() - Should create TrustedForwarderChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
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

        test("handleSuperTokenMinimumDepositChanged() - Should create SuperTokenMinimumDepositChangedEvent entity", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
            const isKeySet = true;
            const minimumDeposit = BigInt.fromI32(69);

            const superTokenMinimumDepositChangedEvent = createSuperTokenMinimumDepositChangedEvent(
                host,
                superToken,
                isKeySet,
                minimumDeposit
            );

            handleSuperTokenMinimumDepositChanged(superTokenMinimumDepositChangedEvent);

            const id = assertEventBaseProperties(
                superTokenMinimumDepositChangedEvent,
                "SuperTokenMinimumDepositChanged"
            );

            assert.fieldEquals("SuperTokenMinimumDepositChangedEvent", id, "host", host);
            assert.fieldEquals("SuperTokenMinimumDepositChangedEvent", id, "superToken", superToken);
            assert.fieldEquals("SuperTokenMinimumDepositChangedEvent", id, "isKeySet", TRUE);
            assert.fieldEquals("SuperTokenMinimumDepositChangedEvent", id, "minimumDeposit", minimumDeposit.toString());
        });
    });

    
    describe("Higher Order Level Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();

            const mockEvent = newMockEvent();
            createSuperToken(
                Address.fromString(maticXAddress),
                mockEvent.block,
                DEFAULT_DECIMALS,
                maticXName,
                maticXSymbol,
                false,
                Address.zero()
            );
        });

        test("handleRewardAddressChanged() - Should modify TokenGovernanceConfig entity field: rewardAddress", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
            const isKeySet = true;
            const rewardAddress = charlie;

            const rewardAddressChangedEvent = createRewardAddressChangedEvent(
                host,
                superToken,
                isKeySet,
                rewardAddress
            );

            handleRewardAddressChanged(rewardAddressChangedEvent);

            assert.fieldEquals("TokenGovernanceConfig", maticXAddress, "rewardAddress", charlie);
        });

        test("handleRewardAddressChanged() - Should create TokenGovernanceConfig (default) entity and modify field: rewardAddress", () => {
            const host = hostAddress;
            const isKeySet = true;
            const superToken = STRING_ZERO_ADDRESS;
            const rewardAddress = charlie;

            const rewardAddressChangedEvent = createRewardAddressChangedEvent(
                host,
                STRING_ZERO_ADDRESS,
                isKeySet,
                rewardAddress
            );

            handleRewardAddressChanged(rewardAddressChangedEvent);

            assert.fieldEquals("TokenGovernanceConfig", superToken, "rewardAddress", charlie);
            assert.fieldEquals("TokenGovernanceConfig", superToken, "isDefault", TRUE);
        });

        test("handleCFAv1LiquidationPeriodChanged() - Should modify TokenGovernanceConfig entity field: liquidationPeriod", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
            const isKeySet = true;

            const cfaV1LiquidationPeriodChangedEvent =
                createCFAv1LiquidationPeriodChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    LIQUIDATION_PERIOD
                );

            handleCFAv1LiquidationPeriodChanged(
                cfaV1LiquidationPeriodChangedEvent
            );

            assert.fieldEquals("TokenGovernanceConfig", maticXAddress, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
        });

        test("handleCFAv1LiquidationPeriodChanged() - Should create TokenGovernanceConfig (default) entity and modify field: liquidationPeriod", () => {
            const host = hostAddress;
            const superToken = STRING_ZERO_ADDRESS;
            const isKeySet = true;

            const cfaV1LiquidationPeriodChangedEvent =
                createCFAv1LiquidationPeriodChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    LIQUIDATION_PERIOD
                );

            handleCFAv1LiquidationPeriodChanged(
                cfaV1LiquidationPeriodChangedEvent
            );

            assert.fieldEquals(
                "TokenGovernanceConfig",
                STRING_ZERO_ADDRESS,
                "liquidationPeriod",
                LIQUIDATION_PERIOD.toString()
            );
            assert.fieldEquals("TokenGovernanceConfig", superToken, "isDefault", TRUE);
        });

        test("handlePPPConfigurationChanged() - Should create TokenGovernanceConfig entity and modify fields: liquidationPeriod and patricianPeriod", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
            const isKeySet = true;

            const pppConfigurationChangedEvent =
                createPPPConfigurationChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    LIQUIDATION_PERIOD,
                    PATRICIAN_PERIOD
                );

            handlePPPConfigurationChanged(pppConfigurationChangedEvent);

            assert.fieldEquals("TokenGovernanceConfig", superToken, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
            assert.fieldEquals("TokenGovernanceConfig", superToken, "patricianPeriod", PATRICIAN_PERIOD.toString());
        });

        test("handlePPPConfigurationChanged() - Should modify TokenGovernanceConfig (default) entity fields: liquidationPeriod and patricianPeriod", () => {
            const host = hostAddress;
            const superToken = STRING_ZERO_ADDRESS;
            const isKeySet = true;

            const pppConfigurationChangedEvent =
                createPPPConfigurationChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    LIQUIDATION_PERIOD,
                    PATRICIAN_PERIOD
                );

            handlePPPConfigurationChanged(pppConfigurationChangedEvent);

            assert.fieldEquals("TokenGovernanceConfig", STRING_ZERO_ADDRESS, "liquidationPeriod", LIQUIDATION_PERIOD.toString());
            assert.fieldEquals("TokenGovernanceConfig", STRING_ZERO_ADDRESS, "patricianPeriod", PATRICIAN_PERIOD.toString());
        });

        test("handleSuperTokenMinimumDepositChanged() - Should modify Token entity field: minimumDeposit", () => {
            const host = hostAddress;
            const superToken = maticXAddress;
            const isKeySet = true;
            const minimumDeposit = BigInt.fromI32(69);

            const SuperTokenMinimumDepositChangedEvent =
                createSuperTokenMinimumDepositChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    minimumDeposit
                );

            handleSuperTokenMinimumDepositChanged(
                SuperTokenMinimumDepositChangedEvent
            );

            assert.fieldEquals("TokenGovernanceConfig", maticXAddress, "minimumDeposit", minimumDeposit.toString());
        });

        test("handleSuperTokenMinimumDepositChanged() - Should create TokenGovernanceConfig (default) entity and modify field: minimumDeposit", () => {
            const host = hostAddress;
            const superToken = STRING_ZERO_ADDRESS;
            const isKeySet = true;
            const minimumDeposit = BigInt.fromI32(69);

            const SuperTokenMinimumDepositChangedEvent =
                createSuperTokenMinimumDepositChangedEvent(
                    host,
                    superToken,
                    isKeySet,
                    minimumDeposit
                );

            handleSuperTokenMinimumDepositChanged(
                SuperTokenMinimumDepositChangedEvent
            );

            assert.fieldEquals("TokenGovernanceConfig", STRING_ZERO_ADDRESS, "minimumDeposit", minimumDeposit.toString());
        });
    });
});
