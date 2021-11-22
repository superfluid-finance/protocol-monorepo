import { AnyAction } from '@reduxjs/toolkit';
import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { TransactionInfo } from '../../baseArg';

import { invalidateCacheTagsForEvent } from './invalidateCacheTagsForEvent';

export const observeAddressForEventsToInvalidateCache = async (
    observeAddress: string,
    transactionInfo: TransactionInfo,
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {
    const framework = await initializedSuperfluidSource.getFramework(
        transactionInfo.chainId
    );
    framework.query.on(
        (events, unsubscribe) => {
            for (const event of events) {
                invalidateCacheTagsForEvent(
                    transactionInfo.chainId,
                    event,
                    dispatch
                );
            }
            unsubscribe();
        },
        3000,
        observeAddress,
        30000
    );
};
