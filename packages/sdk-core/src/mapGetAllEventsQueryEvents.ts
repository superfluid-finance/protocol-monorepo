import * as events from "./events";
import { GetAllEventsQuery } from "./subgraph/queries/getAllEvents.generated";

// Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
const typeGuard = <T>(obj: T) => obj;

export const mapGetAllEventsQueryEvents = (
    queryResponse: GetAllEventsQuery
): events.AllEvents[] => {
    return queryResponse.events.map((x) => {
        switch (x.__typename) {
            case "AppRegisteredEvent":
                return typeGuard<events.AppRegisteredEvent>({
                    name: "AppRegistered",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    app: x.app,
                });
            case "AgreementClassRegisteredEvent":
                return typeGuard<events.AgreementClassRegisteredEvent>({
                    name: "AgreementClassRegistered",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    agreementType: x.agreementType,
                    code: x.code,
                });
            case "AgreementClassUpdatedEvent":
                return typeGuard<events.AgreementClassUpdatedEvent>({
                    name: "AgreementClassUpdated",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    agreementType: x.agreementType,
                    code: x.code,
                });
            case "AgreementLiquidatedByEvent":
                return typeGuard<events.AgreementLiquidatedByEvent>({
                    name: "AgreementLiquidatedBy",
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
            case "BurnedEvent":
                return typeGuard<events.BurnedEvent>({
                    name: "Burned",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    isSet: x.isSet,
                    liquidationPeriod: Number(x.liquidationPeriod),
                    superToken: x.superToken,
                }) as events.CFAv1LiquidationPeriodChangedEvent;
            case "ConfigChangedEvent":
                return typeGuard<events.ConfigChangedEvent>({
                    name: "ConfigChanged",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    isSet: x.isSet,
                    key: x.key,
                    superToken: x.superToken,
                    value: x.value,
                }) as events.ConfigChangedEvent;
            case "CustomSuperTokenCreatedEvent":
                return typeGuard<events.CustomSuperTokenCreatedEvent>({
                    name: "CustomSuperTokenCreated",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                }) as events.CustomSuperTokenCreatedEvent;
            case "FlowUpdatedEvent":
                return typeGuard<events.FlowUpdatedEvent>({
                    name: "FlowUpdated",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    oldGovernance: x.oldGovernance,
                    newGovernance: x.newGovernance,
                });
            case "IndexCreatedEvent":
                return typeGuard<events.IndexCreatedEvent>({
                    name: "IndexCreated",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    app: x.app,
                    reason: x.reason,
                });
            case "MintedEvent":
                return typeGuard<events.MintedEvent>({
                    name: "Minted",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    superToken: x.superToken,
                    isSet: x.isSet,
                    rewardAddress: x.rewardAddress,
                });
            case "RoleAdminChangedEvent":
                return typeGuard<events.RoleAdminChangedEvent>({
                    name: "RoleAdminChanged",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                });
            case "SuperTokenFactoryUpdatedEvent":
                return typeGuard<events.SuperTokenFactoryUpdatedEvent>({
                    name: "SuperTokenFactoryUpdated",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    newFactory: x.newFactory,
                });
            case "SuperTokenLogicCreatedEvent":
                return typeGuard<events.SuperTokenLogicCreatedEvent>({
                    name: "SuperTokenLogicCreated",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    tokenLogic: x.tokenLogic,
                });
            case "SuperTokenLogicUpdatedEvent":
                return typeGuard<events.SuperTokenLogicUpdatedEvent>({
                    name: "SuperTokenLogicUpdated",
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    token: x.token,
                    code: x.code,
                }) as events.SuperTokenLogicUpdatedEvent;
            case "TokenDowngradedEvent":
                return typeGuard<events.TokenDowngradedEvent>({
                    name: "TokenDowngraded",
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
                    blockNumber: Number(x.blockNumber),
                    transactionHash: x.transactionHash,
                    timestamp: Number(x.timestamp),
                    host: x.host,
                    superToken: x.superToken,
                    isSet: x.isSet,
                    forwarder: x.forwarder,
                    enabled: x.enabled,
                });
            default:
                throw Error("Unknown error.");
        }
    });
};
