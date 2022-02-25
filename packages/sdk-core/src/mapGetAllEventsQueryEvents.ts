import * as events from "./events";
import { EventsQuery } from "./subgraph/events/events.generated";
import { typeGuard } from "./utils";

export const mapGetAllEventsQueryEvents = (
    queryEvents: EventsQuery["events"]
): events.AllEvents[] => {
    return queryEvents.map((x) => {
        switch (x.__typename) {
            case "AppRegisteredEvent":
                return typeGuard<events.AppRegisteredEvent>({
                    name: "AppRegistered",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    app: x.app,
                });
            case "AgreementClassRegisteredEvent":
                return typeGuard<events.AgreementClassRegisteredEvent>({
                    name: "AgreementClassRegistered",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    agreementType: x.agreementType,
                    code: x.code,
                });
            case "AgreementClassUpdatedEvent":
                return typeGuard<events.AgreementClassUpdatedEvent>({
                    name: "AgreementClassUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    agreementType: x.agreementType,
                    code: x.code,
                });
            case "AgreementLiquidatedByEvent":
                return typeGuard<events.AgreementLiquidatedByEvent>({
                    name: "AgreementLiquidatedBy",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    agreementClass: x.agreementClass,
                    agreementId: x.agreementId,
                    bailoutAmount: x.bailoutAmount,
                    bondAccount: x.bondAccount,
                    liquidatorAccount: x.liquidatorAccount,
                    penaltyAccount: x.penaltyAccount,
                    rewardAmount: x.rewardAmount,
                    token: x.token,
                });
            case "AgreementLiquidatedV2Event":
                return typeGuard<events.AgreementLiquidatedV2Event>({
                    name: "AgreementLiquidatedV2",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    liquidatorAccount: x.liquidatorAccount,
                    agreementClass: x.agreementClass,
                    agreementId: x.agreementId,
                    targetAccount: x.targetAccount,
                    rewardAccount: x.rewardAccount,
                    rewardAmount: x.rewardAmount,
                    targetAccountBalanceDelta: x.targetAccountBalanceDelta,
                    version: x.version,
                    liquidationType: x.liquidationType,
                });
            case "BurnedEvent":
                return typeGuard<events.BurnedEvent>({
                    name: "Burned",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    amount: x.amount,
                    data: x.data,
                    from: x.from,
                    operator: x.operator,
                    operatorData: x.operatorData,
                });
            case "CFAv1LiquidationPeriodChangedEvent":
                return typeGuard<events.CFAv1LiquidationPeriodChangedEvent>({
                    name: "CFAv1LiquidationPeriodChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    isKeySet: x.isKeySet,
                    liquidationPeriod: Number(x.liquidationPeriod),
                    superToken: x.superToken,
                }) as events.CFAv1LiquidationPeriodChangedEvent;
            case "ConfigChangedEvent":
                return typeGuard<events.ConfigChangedEvent>({
                    name: "ConfigChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    isKeySet: x.isKeySet,
                    key: x.key,
                    superToken: x.superToken,
                    value: x.value,
                }) as events.ConfigChangedEvent;
            case "CustomSuperTokenCreatedEvent":
                return typeGuard<events.CustomSuperTokenCreatedEvent>({
                    name: "CustomSuperTokenCreated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                }) as events.CustomSuperTokenCreatedEvent;
            case "FlowUpdatedEvent":
                return typeGuard<events.FlowUpdatedEvent>({
                    name: "FlowUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    flowRate: x.flowRate,
                    receiver: x.receiver,
                    sender: x.sender,
                });
            case "GovernanceReplacedEvent":
                return typeGuard<events.GovernanceReplacedEvent>({
                    name: "GovernanceReplaced",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    oldGovernance: x.oldGovernance,
                    newGovernance: x.newGovernance,
                });
            case "IndexCreatedEvent":
                return typeGuard<events.IndexCreatedEvent>({
                    name: "IndexCreated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    userData: x.userData,
                });
            case "IndexDistributionClaimedEvent":
                return typeGuard<events.IndexDistributionClaimedEvent>({
                    name: "IndexDistributionClaimed",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    amount: x.amount,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                });
            case "IndexSubscribedEvent":
                return typeGuard<events.IndexSubscribedEvent>({
                    name: "IndexSubscribed",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                });
            case "IndexUnitsUpdatedEvent":
                return typeGuard<events.IndexUnitsUpdatedEvent>({
                    name: "IndexUnitsUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                    units: x.units,
                    oldUnits: x.oldUnits,
                    userData: x.userData,
                });
            case "IndexUnsubscribedEvent":
                return typeGuard<events.IndexUnsubscribedEvent>({
                    name: "IndexUnsubscribed",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                    userData: x.userData,
                });
            case "IndexUpdatedEvent":
                return typeGuard<events.IndexUpdatedEvent>({
                    name: "IndexUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    indexId: x.indexId,
                    newIndexValue: x.newIndexValue,
                    oldIndexValue: x.oldIndexValue,
                    publisher: x.publisher,
                    totalUnitsApproved: x.totalUnitsApproved,
                    totalUnitsPending: x.totalUnitsPending,
                    userData: x.userData,
                });
            case "JailEvent":
                return typeGuard<events.JailEvent>({
                    name: "Jail",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    app: x.app,
                    reason: x.reason,
                });
            case "MintedEvent":
                return typeGuard<events.MintedEvent>({
                    name: "Minted",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    operator: x.operator,
                    to: x.to,
                    amount: x.amount,
                    data: x.data,
                    operatorData: x.operatorData,
                });
            case "RewardAddressChangedEvent":
                return typeGuard<events.RewardAddressChangedEvent>({
                    name: "RewardAddressChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    superToken: x.superToken,
                    isKeySet: x.isKeySet,
                    rewardAddress: x.rewardAddress,
                });
            case "RoleAdminChangedEvent":
                return typeGuard<events.RoleAdminChangedEvent>({
                    name: "RoleAdminChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    role: x.role,
                    previousAdminRole: x.previousAdminRole,
                    newAdminRole: x.newAdminRole,
                });
            case "RoleGrantedEvent":
                return typeGuard<events.RoleGrantedEvent>({
                    name: "RoleGranted",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    role: x.role,
                    account: x.account,
                    sender: x.sender,
                });
            case "RoleRevokedEvent":
                return typeGuard<events.RoleRevokedEvent>({
                    name: "RoleRevoked",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    role: x.role,
                    account: x.account,
                    sender: x.sender,
                });
            case "SentEvent":
                return typeGuard<events.SentEvent>({
                    name: "Sent",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    operator: x.operator,
                    to: x.to,
                    amount: x.amount,
                    data: x.data,
                    operatorData: x.operatorData,
                });
            case "SubscriptionApprovedEvent":
                return typeGuard<events.SubscriptionApprovedEvent>({
                    name: "SubscriptionApproved",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    subscriber: x.subscriber,
                    publisher: x.publisher,
                    indexId: x.indexId,
                    userData: x.userData,
                    subscription: x.subscription.id,
                });
            case "SubscriptionDistributionClaimedEvent":
                return typeGuard<events.SubscriptionDistributionClaimedEvent>({
                    name: "SubscriptionDistributionClaimed",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    subscriber: x.subscriber,
                    publisher: x.publisher,
                    indexId: x.indexId,
                    amount: x.amount,
                    subscription: x.subscription.id,
                });
            case "SubscriptionRevokedEvent":
                return typeGuard<events.SubscriptionRevokedEvent>({
                    name: "SubscriptionRevoked",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    subscriber: x.subscriber,
                    publisher: x.publisher,
                    indexId: x.indexId,
                    userData: x.userData,
                    subscription: x.subscription.id,
                });
            case "SubscriptionUnitsUpdatedEvent":
                return typeGuard<events.SubscriptionUnitsUpdatedEvent>({
                    name: "SubscriptionUnitsUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    subscriber: x.subscriber,
                    publisher: x.publisher,
                    indexId: x.indexId,
                    units: x.units,
                    userData: x.userData,
                    subscription: x.subscription.id,
                });
            case "SuperTokenCreatedEvent":
                return typeGuard<events.SuperTokenCreatedEvent>({
                    name: "SuperTokenCreated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                });
            case "SuperTokenFactoryUpdatedEvent":
                return typeGuard<events.SuperTokenFactoryUpdatedEvent>({
                    name: "SuperTokenFactoryUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    newFactory: x.newFactory,
                });
            case "SuperTokenLogicCreatedEvent":
                return typeGuard<events.SuperTokenLogicCreatedEvent>({
                    name: "SuperTokenLogicCreated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    tokenLogic: x.tokenLogic,
                });
            case "SuperTokenLogicUpdatedEvent":
                return typeGuard<events.SuperTokenLogicUpdatedEvent>({
                    name: "SuperTokenLogicUpdated",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    code: x.code,
                }) as events.SuperTokenLogicUpdatedEvent;
            case "PPPConfigurationChangedEvent":
                return typeGuard<events.PPPConfigurationChangedEvent>({
                    name: "PPPConfigurationChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    superToken: x.superToken,
                    isKeySet: x.isKeySet,
                    liquidationPeriod: x.liquidationPeriod,
                    patricianPeriod: x.patricianPeriod,
                });
            case "TokenDowngradedEvent":
                return typeGuard<events.TokenDowngradedEvent>({
                    name: "TokenDowngraded",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    amount: x.amount,
                    token: x.token,
                    account: x.account.id,
                });
            case "TokenUpgradedEvent":
                return typeGuard<events.TokenUpgradedEvent>({
                    name: "TokenUpgraded",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    amount: x.amount,
                    token: x.token,
                    account: x.account.id,
                });
            case "TransferEvent":
                return typeGuard<events.TransferEvent>({
                    name: "Transfer",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    from: x.from.id,
                    to: x.to.id,
                    token: x.token,
                    value: x.value,
                });
            case "TrustedForwarderChangedEvent":
                return typeGuard<events.TrustedForwarderChangedEvent>({
                    name: "TrustedForwarderChanged",
                    id: x.id,
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    superToken: x.superToken,
                    isKeySet: x.isKeySet,
                    forwarder: x.forwarder,
                    enabled: x.enabled,
                });
            default:
                throw Error("Unknown error.");
        }
    });
};
