import {AnyAction, ThunkDispatch} from '@reduxjs/toolkit';
import {AllEvents} from '@superfluid-finance/sdk-core';

import {getApiSlice, getSubgraphSlice} from '../../../sdkReduxConfig';

import {createEventTag} from './eventTags';
import {createIndexTags} from './indexTags';
import {createStreamsTags} from './streamTags';
import {createTokenTags} from './tokenTags';

/**
 * Based on event type, invalidate all possible relevant cache tags.
 * Cache tag invalidation will trigger re-querying of data.
 * @private
 * @category Cache Tags
 */
export const invalidateCacheTagsForEvents = (
    chainId: number,
    events: AllEvents[],
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {
    const tagsToInvalidate = events
        .map((event) => [createEventTag(chainId), ...getEventSpecificTags(event, chainId)])
        .flat();

    dispatch(getApiSlice().util.invalidateTags(tagsToInvalidate));
    dispatch(getSubgraphSlice().util.invalidateTags(tagsToInvalidate));
};

const getEventSpecificTags = (event: AllEvents, chainId: number) => {
    switch (event.name) {
        case 'SubscriptionApproved':
        case 'IndexDistributionClaimed':
        case 'IndexSubscribed':
        case 'IndexUnitsUpdated':
        case 'IndexUnsubscribed':
        case 'SubscriptionDistributionClaimed':
        case 'SubscriptionRevoked':
        case 'SubscriptionUnitsUpdated':
            return createIndexTags({
                chainId,
                address1: event.token,
                address2: event.publisher,
                address3: event.subscriber,
                indexId: event.indexId,
            });
        case 'IndexCreated':
        case 'IndexUpdated':
            return createIndexTags({
                chainId,
                address1: event.token,
                address2: event.publisher,
                address3: undefined,
                indexId: event.indexId,
            });
        case 'FlowUpdated':
            return createStreamsTags({
                chainId,
                address1: event.token,
                address2: event.sender,
                address3: event.receiver,
            });
        case 'TokenUpgraded':
        case 'TokenDowngraded':
            return createTokenTags({
                chainId,
                address1: event.token,
                address2: event.account,
                address3: undefined,
            });
        case 'Transfer':
            return createTokenTags({
                chainId,
                address1: event.token,
                address2: event.from,
                address3: event.to,
            });
        case 'AgreementLiquidatedBy':
            return createStreamsTags({
                chainId,
                address1: event.token,
                address2: event.penaltyAccount,
                address3: undefined,
            });
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
            return createTokenTags({
                chainId,
                address1: event.token,
                address3: undefined,
                address2: undefined,
            });
        case 'GovernanceReplaced':
            return [];
        case 'Jail':
            return [];
        case 'Minted':
            return createTokenTags({
                chainId,
                address1: event.to,
                address2: undefined,
                address3: undefined,
            });
        case 'RewardAddressChanged':
            return [];
        case 'RoleAdminChanged':
            return [];
        case 'RoleGranted':
            return [];
        case 'RoleRevoked':
            return [];
        case 'Sent':
            return createTokenTags({
                chainId,
                address1: event.to,
                address2: undefined,
                address3: undefined,
            });
        case 'SuperTokenLogicUpdated':
        case 'SuperTokenCreated':
            return createTokenTags({
                chainId,
                address1: event.token,
                address2: undefined,
                address3: undefined,
            });
        case 'SuperTokenFactoryUpdated':
            return [];
        case 'SuperTokenLogicCreated':
            return [];
        case 'TrustedForwarderChanged':
            return createTokenTags({
                chainId,
                address1: event.superToken,
                address2: undefined,
                address3: undefined,
            });
        default:
            throw Error('Unknown event type!');
    }
};
