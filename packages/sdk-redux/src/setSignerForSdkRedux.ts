import {Signer} from 'ethers';

import {getConfig} from './sdkReduxConfig';

/**
 * SDK-Redux needs to know where to get the signer for mutations (i.e. transaction signing & broadcasting)
 * @param chainId The chain.
 * @param signer The Ethers signer getter. Can be either Promise or instance.
 */
export const setSignerForSdkRedux = (chainId: number, signer: (() => Promise<Signer>) | Signer) =>
    getConfig().setSigner(chainId, signer);
