import {Framework} from '@superfluid-finance/sdk-core';

import {getConfig} from './sdkReduxConfig';

/**
 * SDK-Redux needs to know where to get SDK-Core's Framework for data querying.
 * @param chainId The chain.
 * @param framework The SDK-Framework getter. Can be either Promise or instance.
 */
export const setFrameworkForSdkRedux = (chainId: number, framework: (() => Promise<Framework>) | Framework) =>
    getConfig().setFramework(chainId, framework);
