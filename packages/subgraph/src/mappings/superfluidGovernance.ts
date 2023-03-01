import {
    ConfigChanged,
    RewardAddressChanged,
    CFAv1LiquidationPeriodChanged,
    TrustedForwarderChanged,
    PPPConfigurationChanged,
    SuperTokenMinimumDepositChanged,
} from "../../generated/templates/SuperfluidGovernance/SuperfluidGovernanceBase";
import {
    CFAv1LiquidationPeriodChangedEvent,
    ConfigChangedEvent,
    RewardAddressChangedEvent,
    PPPConfigurationChangedEvent,
    TrustedForwarderChangedEvent,
    Token,
    DefaultGovernanceConfigs,
    SuperTokenMinimumDepositChangedEvent,
} from "../../generated/schema";
import { createEventID, initializeEventEntity, ZERO_ADDRESS } from "../utils";
import { TOGA } from "../../generated/templates";
import { getOrInitDefaultGovernanceConfigs } from "../mappingHelpers";

export function handleConfigChanged(event: ConfigChanged): void {
    const eventId = createEventID("ConfigChanged", event);
    const ev = new ConfigChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.key = event.params.key;
    ev.isKeySet = event.params.isKeySet;
    ev.value = event.params.value;
    ev.save();
}

export function handleRewardAddressChanged(event: RewardAddressChanged): void {
    const eventId = createEventID("RewardAddressChanged", event);
    const ev = new RewardAddressChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.rewardAddress = event.params.rewardAddress;
    ev.save();

    // Update reward address for token or default governance configs
    if (event.params.superToken.equals(ZERO_ADDRESS)) {
        let defaultGovernanceConfigs = DefaultGovernanceConfigs.load(
            ZERO_ADDRESS.toHexString()
        );
        if (defaultGovernanceConfigs) {
            defaultGovernanceConfigs.rewardAddress = event.params.rewardAddress;
        } else {
            defaultGovernanceConfigs = getOrInitDefaultGovernanceConfigs(
                event.block
            );
            defaultGovernanceConfigs.rewardAddress = event.params.rewardAddress;
        }

        defaultGovernanceConfigs.save();
    } else {
        const token = Token.load(event.params.superToken.toHexString());
        if (token) {
            token.rewardAddress = event.params.rewardAddress;
            token.save();
        }
    }

    // Create data source template for new TOGA contract
    // and start indexing events
    // @note The subgraph will start capturing TOGA events
    // which occur once this event is emitted for a valid TOGA address
    TOGA.create(event.params.rewardAddress);
}

export function handleCFAv1LiquidationPeriodChanged(
    event: CFAv1LiquidationPeriodChanged
): void {
    const eventId = createEventID("CFAv1LiquidationPeriodChanged", event);
    const ev = new CFAv1LiquidationPeriodChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.liquidationPeriod = event.params.liquidationPeriod;
    ev.save();

    // Update cfav1 liquidation period for token or default governance configs
    if (event.params.superToken.equals(ZERO_ADDRESS)) {
        let defaultGovernanceConfigs = DefaultGovernanceConfigs.load(
            ZERO_ADDRESS.toHexString()
        );
        if (defaultGovernanceConfigs) {
            defaultGovernanceConfigs.liquidationPeriod =
                event.params.liquidationPeriod;
        } else {
            defaultGovernanceConfigs = getOrInitDefaultGovernanceConfigs(
                event.block
            );
            defaultGovernanceConfigs.liquidationPeriod =
                event.params.liquidationPeriod;
        }

        defaultGovernanceConfigs.save();
    } else {
        const token = Token.load(event.params.superToken.toHexString());
        if (token) {
            token.liquidationPeriod = event.params.liquidationPeriod;
            token.save();
        }
    }
}

export function handlePPPConfigurationChanged(
    event: PPPConfigurationChanged
): void {
    const eventId = createEventID("PPPConfigurationChanged", event);
    const ev = new PPPConfigurationChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.liquidationPeriod = event.params.liquidationPeriod;
    ev.patricianPeriod = event.params.patricianPeriod;
    ev.save();

    // Update cfav1 liquidation period and patrician period for token or default governance configs
    if (event.params.superToken.equals(ZERO_ADDRESS)) {
        let defaultGovernanceConfigs = DefaultGovernanceConfigs.load(
            ZERO_ADDRESS.toHexString()
        );
        if (defaultGovernanceConfigs) {
            defaultGovernanceConfigs.liquidationPeriod =
                event.params.liquidationPeriod;
            defaultGovernanceConfigs.patricianPeriod =
                event.params.patricianPeriod;
        } else {
            defaultGovernanceConfigs = getOrInitDefaultGovernanceConfigs(
                event.block
            );
            defaultGovernanceConfigs.liquidationPeriod =
                event.params.liquidationPeriod;
            defaultGovernanceConfigs.patricianPeriod =
                event.params.patricianPeriod;
        }

        defaultGovernanceConfigs.save();
    } else {
        const token = Token.load(event.params.superToken.toHexString());
        if (token) {
            token.liquidationPeriod = event.params.liquidationPeriod;
            token.patricianPeriod = event.params.patricianPeriod;
            token.save();
        }
    }
}

export function handleTrustedForwarderChanged(
    event: TrustedForwarderChanged
): void {
    const eventId = createEventID("TrustedForwarderChanged", event);
    const ev = new TrustedForwarderChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.forwarder = event.params.forwarder;
    ev.enabled = event.params.enabled;
    ev.save();
}

export function handleSuperTokenMinimumDepositChanged(
    event: SuperTokenMinimumDepositChanged
): void {
    const eventId = createEventID("SuperTokenMinimumDepositChanged", event);
    const ev = new SuperTokenMinimumDepositChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.minimumDeposit = event.params.minimumDeposit;
    ev.save();

    // Update minimum deposit for token or default governance configs
    if (event.params.superToken.equals(ZERO_ADDRESS)) {
        let defaultGovernanceConfigs = DefaultGovernanceConfigs.load(
            ZERO_ADDRESS.toHexString()
        );
        if (defaultGovernanceConfigs) {
            defaultGovernanceConfigs.minimumDeposit =
                event.params.minimumDeposit;
        } else {
            defaultGovernanceConfigs = getOrInitDefaultGovernanceConfigs(
                event.block
            );
            defaultGovernanceConfigs.minimumDeposit =
                event.params.minimumDeposit;
        }

        defaultGovernanceConfigs.save();
    } else {
        const token = Token.load(event.params.superToken.toHexString());
        if (token) {
            token.minimumDeposit = event.params.minimumDeposit;
            token.save();
        }
    }
}