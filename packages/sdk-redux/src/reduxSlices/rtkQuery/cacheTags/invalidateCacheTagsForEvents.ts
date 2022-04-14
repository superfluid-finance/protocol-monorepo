import {AnyAction, ThunkDispatch} from '@reduxjs/toolkit';
import {AllEvents} from '@superfluid-finance/sdk-core';
import {uniqBy} from 'lodash';

import {getRpcApiSlice, getSubgraphApiSlice} from '../../../sdkReduxConfig';

import {createTags} from './CacheTagTypes';

/**
 * Get all the possible event tags. Run a deduplication to avoid unnecessary work in the redux store.
 */
export const getCacheTagsToInvalidateForEvents = (chainId: number, events: AllEvents[]) =>
    uniqBy(events.map((event) => [...getEventSpecificTags(event, chainId)]).flat(), (tag) => tag.type + tag.id);

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
    const tagsToInvalidate = getCacheTagsToInvalidateForEvents(chainId, events);

    dispatch(getRpcApiSlice().util.invalidateTags(tagsToInvalidate));
    dispatch(getSubgraphApiSlice().util.invalidateTags(tagsToInvalidate));
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
            return createTags(['Index'], {
                chainId,
                address1: event.token,
                address2: event.publisher,
                address3: event.subscriber,
            });
        case 'IndexCreated':
        case 'IndexUpdated':
            return createTags(['Index', 'Balance'], {
                chainId,
                address1: event.token,
                address2: event.publisher,
            });
        case 'FlowUpdated':
            return createTags(['Stream', 'Balance'], {
                chainId,
                address1: event.token,
                address2: event.sender,
                address3: event.receiver,
            });
        case 'TokenUpgraded':
        case 'TokenDowngraded':
            return createTags(['Balance'], {
                chainId,
                address1: event.token,
                address2: event.account,
            });
        case 'Transfer':
            return createTags(['Balance'], {
                chainId,
                address1: event.token,
                address2: event.from,
                address3: event.to,
            });
        case 'AgreementLiquidatedV2':
            return [
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.liquidatorAccount,
                }),
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.rewardAccount,
                }),
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.targetAccount,
                }),
            ].flat();
        case 'AgreementLiquidatedBy':
            return [
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.penaltyAccount,
                }),
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.bondAccount,
                }),
                createTags(['Balance'], {
                    chainId,
                    address1: event.token,
                    address2: event.rewardAmount,
                }),
            ].flat();
        case 'CustomSuperTokenCreated':
            return createTags(['TokenList', 'Balance'], {
                chainId,
                address1: event.token,
            });
        case 'Minted':
            return createTags(['TokenList', 'Balance'], {
                chainId,
                address1: event.to,
            });
        case 'Sent':
            return createTags(['Balance'], {
                chainId,
                address1: event.to,
            });
        case 'SuperTokenLogicUpdated':
        case 'SuperTokenCreated':
            return createTags([], {
                chainId,
                address1: event.token,
            });
        case 'TrustedForwarderChanged':
            return createTags([], {
                chainId,
                address1: event.superToken,
            });
        case 'AgreementClassRegistered':
        case 'AgreementClassUpdated':
        case 'AppRegistered':
        case 'Burned':
        case 'CFAv1LiquidationPeriodChanged':
        case 'ConfigChanged':
        case 'PPPConfigurationChanged':
        case 'GovernanceReplaced':
        case 'Jail':
        case 'RewardAddressChanged':
        case 'RoleAdminChanged':
        case 'RoleGranted':
        case 'RoleRevoked':
        case 'SuperTokenFactoryUpdated':
        case 'SuperTokenLogicCreated':
            return [];
        default:
            console.warn(
                `Unknown event [${
                    (event as any)?.name
                }] in "invalidateCacheTagsForEvents" for @superfluid-finance/sdk-redux. Cache might not be invalidated properly.`
            );
            return [];
    }
};
