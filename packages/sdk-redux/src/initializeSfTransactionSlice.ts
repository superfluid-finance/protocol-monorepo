import {createTransactionSlice} from './redux-slices/transactions/createTransactionSlice';
import {getConfig} from './sdkReduxConfig';

/**
 * For initializing "sfTransaction" Redux slice.
 */
export const initializeSfTransactionSlice = () => {
    const slice = createTransactionSlice();
    getConfig().setTransactionSlice(slice);
    return slice;
};
