import {RelevantAddresses} from '@superfluid-finance/sdk-core';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

export const provideTagsFromRelevantAddresses = (
    chainId: number,
    relevantAddresses: RelevantAddresses,
    tag: CacheTagTypes
) => {
    if (relevantAddresses.tokens) {
        return relevantAddresses.tokens
            .map((tokenAddress: string) =>
                relevantAddresses.accounts.map((accountAddress: string) => ({
                    type: tag,
                    id: `${chainId}_${tokenAddress}_${accountAddress}`,
                }))
            )
            .flat();
    } else {
        return relevantAddresses.accounts.map((accountAddress: string) => ({
            type: tag,
            id: `${chainId}_${accountAddress}`,
        }));
    }
};
