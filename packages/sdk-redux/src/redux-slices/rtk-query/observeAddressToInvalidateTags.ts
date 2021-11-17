import { AnyAction } from '@reduxjs/toolkit';
import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedSuperfluidSource } from '../../superfluidApi';
import { TransactionInfo } from '../baseArg';

import { invalidateTagsHandler } from './invalidateTagsHandler';

export const observeAddressToInvalidateTags = async (
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
                invalidateTagsHandler(transactionInfo.chainId, event, dispatch);
            }
            unsubscribe();
        },
        2000,
        observeAddress,
        30000
    );
};
