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
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "AgreementClassRegisteredEvent":
                return typeGuard<events.AgreementClassRegisteredEvent>({
                    name: "AgreementClassRegistered",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "AgreementClassUpdatedEvent":
                return typeGuard<events.AgreementClassUpdatedEvent>({
                    name: "AgreementClassUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "AgreementLiquidatedByEvent":
                return typeGuard<events.AgreementLiquidatedByEvent>({
                    name: "AgreementLiquidatedBy",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "BurnedEvent":
                return typeGuard<events.BurnedEvent>({
                    name: "Burned",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "CFAv1LiquidationPeriodChangedEvent":
                return typeGuard<events.CFAv1LiquidationPeriodChangedEvent>({
                    name: "CFAv1LiquidationPeriodChanged",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                }) as events.CFAv1LiquidationPeriodChangedEvent;
            case "ConfigChangedEvent":
                return typeGuard<events.ConfigChangedEvent>({
                    name: "ConfigChanged",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                }) as events.ConfigChangedEvent;
            case "CustomSuperTokenCreatedEvent":
                return typeGuard<events.CustomSuperTokenCreatedEvent>({
                    name: "CustomSuperTokenCreated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                }) as events.CustomSuperTokenCreatedEvent;
            case "FlowUpdatedEvent":
                return typeGuard<events.FlowUpdatedEvent>({
                    name: "FlowUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    token: x.token,
                    flowRate: x.flowRate,
                    receiver: x.receiver,
                    sender: x.sender,
                });
            case "GovernanceReplacedEvent":
                return typeGuard<events.GovernanceReplacedEvent>({
                    name: "GovernanceReplaced",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "IndexCreatedEvent":
                return typeGuard<events.IndexCreatedEvent>({
                    name: "IndexCreated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    userData: x.userData,
                });
            case "IndexDistributionClaimedEvent":
                return typeGuard<events.IndexDistributionClaimedEvent>({
                    name: "IndexDistributionClaimed",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    token: x.token,
                    amount: x.amount,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                });
            case "IndexSubscribedEvent":
                return typeGuard<events.IndexSubscribedEvent>({
                    name: "IndexSubscribed",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                });
            case "IndexUnitsUpdatedEvent":
                return typeGuard<events.IndexUnitsUpdatedEvent>({
                    name: "IndexUnitsUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
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
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    token: x.token,
                    indexId: x.indexId,
                    publisher: x.publisher,
                    subscriber: x.subscriber,
                    userData: x.userData,
                });
            case "IndexUpdatedEvent":
                return typeGuard<events.IndexUpdatedEvent>({
                    name: "IndexUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
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
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "MintedEvent":
                return typeGuard<events.MintedEvent>({
                    name: "Minted",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "RewardAddressChangedEvent":
                return typeGuard<events.RewardAddressChangedEvent>({
                    name: "RewardAddressChanged",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "RoleAdminChangedEvent":
                return typeGuard<events.RoleAdminChangedEvent>({
                    name: "RoleAdminChanged",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "RoleGrantedEvent":
                return typeGuard<events.RoleGrantedEvent>({
                    name: "RoleGranted",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "RoleRevokedEvent":
                return typeGuard<events.RoleRevokedEvent>({
                    name: "RoleRevoked",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SentEvent":
                return typeGuard<events.SentEvent>({
                    name: "Sent",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SubscriptionApprovedEvent":
                return typeGuard<events.SubscriptionApprovedEvent>({
                    name: "SubscriptionApproved",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SubscriptionDistributionClaimedEvent":
                return typeGuard<events.SubscriptionDistributionClaimedEvent>({
                    name: "SubscriptionDistributionClaimed",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SubscriptionRevokedEvent":
                return typeGuard<events.SubscriptionRevokedEvent>({
                    name: "SubscriptionRevoked",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SubscriptionUnitsUpdatedEvent":
                return typeGuard<events.SubscriptionUnitsUpdatedEvent>({
                    name: "SubscriptionUnitsUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SuperTokenCreatedEvent":
                return typeGuard<events.SuperTokenCreatedEvent>({
                    name: "SuperTokenCreated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SuperTokenFactoryUpdatedEvent":
                return typeGuard<events.SuperTokenFactoryUpdatedEvent>({
                    name: "SuperTokenFactoryUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SuperTokenLogicCreatedEvent":
                return typeGuard<events.SuperTokenLogicCreatedEvent>({
                    name: "SuperTokenLogicCreated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            case "SuperTokenLogicUpdatedEvent":
                return typeGuard<events.SuperTokenLogicUpdatedEvent>({
                    name: "SuperTokenLogicUpdated",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                }) as events.SuperTokenLogicUpdatedEvent;
            case "TokenDowngradedEvent":
                return typeGuard<events.TokenDowngradedEvent>({
                    name: "TokenDowngraded",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    amount: x.amount,
                    token: x.token,
                });
            case "TokenUpgradedEvent":
                return typeGuard<events.TokenUpgradedEvent>({
                    name: "TokenUpgraded",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    amount: x.amount,
                    token: x.token,
                });
            case "TransferEvent":
                return typeGuard<events.TransferEvent>({
                    name: "Transfer",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                    from: x.from.id,
                    to: x.to.id,
                    token: x.token,
                    value: x.value,
                });
            case "TrustedForwarderChangedEvent":
                return typeGuard<events.TrustedForwarderChangedEvent>({
                    name: "TrustedForwarderChanged",
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                    timestamp: x.timestamp,
                });
            default:
                throw Error("Unknown error.");
        }
    });
};
