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
            },
        ])
    );

    switch (event.name) {
        case 'SubscriptionApproved':
            break;
        case 'FlowUpdated':
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
        case 'IndexCreated':
            break;
        case 'IndexDistributionClaimed':
            break;
        case 'IndexSubscribed':
            break;
        case 'IndexUnitsUpdated':
            break;
        case 'IndexUnsubscribed':
            break;
        case 'IndexUpdated':
            break;
        case 'TokenDowngraded':
            break;
        case 'Transfer':
            break;
        case 'TokenUpgraded':
            break;
        case 'SubscriptionDistributionClaimed':
            break;
        case 'SubscriptionRevoked':
            break;
        case 'SubscriptionUnitsUpdated':
            break;
        case 'AgreementClassRegistered':
            break;
        case 'AgreementClassUpdated':
            break;
        case 'AgreementLiquidatedBy':
            break;
        case 'AppRegistered':
            break;
        case 'Burned':
            break;
        case 'CFAv1LiquidationPeriodChanged':
            break;
        case 'ConfigChanged':
            break;
        case 'CustomSuperTokenCreated':
            break;
        case 'GovernanceReplaced':
            break;
        case 'Jail':
            break;
        case 'Minted':
            break;
        case 'RewardAddressChanged':
            break;
        case 'RoleAdminChanged':
            break;
        case 'RoleGranted':
            break;
        case 'RoleRevoked':
            break;
        case 'Sent':
            break;
        case 'SuperTokenCreated':
            break;
        case 'SuperTokenFactoryUpdated':
            break;
        case 'SuperTokenLogicCreated':
            break;
        case 'SuperTokenLogicUpdated':
            break;
        case 'TrustedForwarderChanged':
            break;
        default:
            throw Error('Unknown event type!');
    }
};
