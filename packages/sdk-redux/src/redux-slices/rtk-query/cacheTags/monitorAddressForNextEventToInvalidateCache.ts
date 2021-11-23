import { AnyAction } from '@reduxjs/toolkit';
import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedContext } from '../../../superfluidApi';
import { TransactionInfo } from '../../baseArg';

import { invalidateCacheTagsForEvents } from './invalidateCacheTagsForEvents';
import { MsTimes } from '../../../utils';

export const monitorAddressForNextEventToInvalidateCache = async (
    address: string,
    transactionInfo: TransactionInfo,
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {
    const framework = await initializedContext.getFramework(
        transactionInfo.chainId
    );
    framework.query.on(
        (events, unsubscribe) => {
            invalidateCacheTagsForEvents(
                transactionInfo.chainId,
                events,
                dispatch
            );
            unsubscribe();
        },
        MsTimes.OneSecond,
        address,
        MsTimes.OneMinute
    );
};
