import { AnyAction, ThunkDispatch } from '@reduxjs/toolkit';
import { AllEvents } from '@superfluid-finance/sdk-core';

import { rtkQuerySlice } from './rtkQuerySlice';

export const invalidateTagsHandler = (
    chainId: number,
    event: AllEvents,
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {

    // TODO: Make it single dispatch every time.
    dispatch(
        rtkQuerySlice.util.invalidateTags([
            {
                type: 'Event',
                id: `${chainId}`.toLowerCase(),
            }
        ])
    );

    switch (event.__typename) {
        case "SubscriptionApprovedEvent":
            break;
        case "FlowUpdatedEvent":
            dispatch(
                rtkQuerySlice.util.invalidateTags([
                    {
                        type: 'Stream',
                        id: `${chainId}_${event.sender}`.toLowerCase(),
                    },
                    {
                        type: 'Stream',
                        id: `${chainId}_${event.receiver}`.toLowerCase(),
                    },
                ])
            );
            break;
        case "IndexCreatedEvent":
            break;
        case "IndexDistributionClaimedEvent":
            break;
        case "IndexSubscribedEvent":
            break;
        case "IndexUnitsUpdatedEvent":
            break;
        case "IndexUnsubscribedEvent":
            break;
        case "IndexUpdatedEvent":
            break;
        case "TokenDowngradedEvent":
            break;
        case "TransferEvent":
            break;
        case "TokenUpgradedEvent":
            break;
        case "SubscriptionDistributionClaimedEvent":
            break;
        case "SubscriptionRevokedEvent":
            break;
        case "SubscriptionUnitsUpdatedEvent":
            break;
        case "AgreementClassRegisteredEvent":
            break;
        case "AgreementClassUpdatedEvent":
            break;
        case "AgreementLiquidatedByEvent":
            break;
        case "AppRegisteredEvent":
            break;
        case "BurnedEvent":
            break;
        case "CFAv1LiquidationPeriodChangedEvent":
            break;
        case "ConfigChangedEvent":
            break;
        case "CustomSuperTokenCreatedEvent":
            break;
        case "GovernanceReplacedEvent":
            break;
        case "JailEvent":
            break;
        case "MintedEvent":
            break;
        case "RewardAddressChangedEvent":
            break;
        case "RoleAdminChangedEvent":
            break;
        case "RoleGrantedEvent":
            break;
        case "RoleRevokedEvent":
            break;
        case "SentEvent":
            break;
        case "SuperTokenCreatedEvent":
            break;
        case "SuperTokenFactoryUpdatedEvent":
            break;
        case "SuperTokenLogicCreatedEvent":
            break;
        case "SuperTokenLogicUpdatedEvent":
            break;
        case "TrustedForwarderChangedEvent":
            break;
        default:
            throw Error('Unknown event type!');
    }
};
