import { AnyAction, ThunkDispatch } from '@reduxjs/toolkit';
import { AllEvents } from '@superfluid-finance/sdk-core';

import {
    eventTag,
    indexTag,
    rtkQuerySlice,
    streamTag,
    tokenTag,
} from './rtkQuerySlice';

export const invalidateTagsHandler = (
    chainId: number,
    event: AllEvents,
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {
    dispatch(
        rtkQuerySlice.util.invalidateTags([
            eventTag(chainId),
            ...getConcreteEventTags(event, chainId),
        ])
    );
};

function getConcreteEventTags(event: AllEvents, chainId: number) {
    switch (event.name) {
        case 'SubscriptionApproved':
        case 'IndexDistributionClaimed':
        case 'IndexSubscribed':
        case 'IndexUnitsUpdated':
        case 'IndexUnsubscribed':
        case 'SubscriptionDistributionClaimed':
        case 'SubscriptionRevoked':
        case 'SubscriptionUnitsUpdated':
            return [
                indexTag(chainId),
                indexTag(chainId, event.token),
                indexTag(chainId, event.publisher),
                indexTag(chainId, event.indexId),
                indexTag(chainId, event.subscriber),
                indexTag(
                    chainId,
                    event.token,
                    event.publisher,
                    event.indexId,
                    event.subscriber
                ),
            ];
        case 'IndexCreated':
        case 'IndexUpdated':
            return [
                indexTag(chainId),
                indexTag(chainId, event.token),
                indexTag(chainId, event.publisher),
                indexTag(chainId, event.indexId),
                indexTag(chainId, event.token, event.publisher, event.indexId),
            ];
        case 'FlowUpdated':
            return [
                streamTag(chainId),
                streamTag(chainId, event.token),
                streamTag(chainId, event.sender),
                streamTag(chainId, event.receiver),
                streamTag(chainId, event.token, event.sender, event.receiver),
            ];
        case 'TokenUpgraded':
        case 'TokenDowngraded':
            return [
                tokenTag(chainId),
                tokenTag(chainId, event.token),
                tokenTag(chainId, event.account),
                tokenTag(chainId, event.token, event.account),
            ];
        case 'Transfer':
            return [
                tokenTag(chainId),
                tokenTag(chainId, event.token),
                tokenTag(chainId, event.from),
                tokenTag(chainId, event.to),
                tokenTag(chainId, event.token, event.from, event.to),
            ];
        case 'AgreementLiquidatedBy':
            return [
                streamTag(chainId),
                streamTag(chainId, event.token),
                streamTag(chainId, event.penaltyAccount),
                streamTag(chainId, event.token, event.penaltyAccount),
            ];
        case 'AgreementClassRegistered':
            return [];
        case 'AgreementClassUpdated':
            return [];
        case 'AppRegistered':
            return [];
        case 'Burned':
            return [];
        case 'CFAv1LiquidationPeriodChanged':
            return [];
        case 'ConfigChanged':
            return [];
        case 'CustomSuperTokenCreated':
            return [tokenTag(chainId), tokenTag(chainId, event.token)];
        case 'GovernanceReplaced':
            return [];
        case 'Jail':
            return [];
        case 'Minted':
            return [tokenTag(chainId), tokenTag(chainId, event.to)];
        case 'RewardAddressChanged':
            return [];
        case 'RoleAdminChanged':
            return [];
        case 'RoleGranted':
            return [];
        case 'RoleRevoked':
            return [];
        case 'Sent':
            return [tokenTag(chainId), tokenTag(chainId, event.to)];
        case 'SuperTokenLogicUpdated':
        case 'SuperTokenCreated':
            return [tokenTag(chainId), tokenTag(chainId, event.token)];
        case 'SuperTokenFactoryUpdated':
            return [];
        case 'SuperTokenLogicCreated':
            return [];
        case 'TrustedForwarderChanged':
            return [tokenTag(chainId), tokenTag(chainId, event.superToken)];
        default:
            throw Error('Unknown event type!');
    }
}
