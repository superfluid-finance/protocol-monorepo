import {RelevantAddresses} from '@superfluid-finance/sdk-core';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

export const provideCacheTagsFromRelevantAddresses = (
    chainId: number,
    relevantAddresses: RelevantAddresses,
    tag: CacheTagTypes
) => {
    if (relevantAddresses.tokens.length) {
        return relevantAddresses.tokens
            .map((tokenAddress: string) =>
                relevantAddresses.accounts.map((accountAddress: string) => ({
                    type: tag,
                    id: `${chainId}_${tokenAddress}_${accountAddress}`.toLowerCase(),
                }))
            )
            .flat();
    } else if (relevantAddresses.accounts.length) {
        return relevantAddresses.accounts.map((accountAddress: string) => ({
            type: tag,
            id: `${chainId}_${accountAddress}`.toLowerCase(),
        }));
    } else {
        return [
            {
                type: tag,
                id: `${chainId}`.toLowerCase(),
            },
        ];
    }
};
