import {createApi} from '@reduxjs/toolkit/query';

/**
 * For creating RTK-Query API (e.g. "sfApi") without React Hooks (i.e. UI-framework agnostic).
 *
 * Read more: https://redux-toolkit.js.org/rtk-query/usage/usage-without-react-hooks
 */
export const createApiWithoutReactHooks = createApi;
