import {RelevantAddresses} from '@superfluid-finance/sdk-core';

import {CacheTagType, createTag} from '../cacheTags/CacheTagTypes';

export const provideCacheTagsFromRelevantAddresses = (
    chainId: number,
    relevantAddresses: RelevantAddresses,
    tag: CacheTagType
) => {
    if (relevantAddresses.tokens.length) {
        return relevantAddresses.tokens
            .map((tokenAddress: string) =>
                [createTag(tag, tokenAddress)].concat(
                    relevantAddresses.accounts.map((accountAddress: string) =>
                        createTag(tag, tokenAddress, accountAddress)
                    )
                )
            )
            .flat();
    } else if (relevantAddresses.accounts.length) {
        return relevantAddresses.accounts.map((accountAddress: string) => createTag(tag, accountAddress));
    } else {
        return [createTag(tag, chainId)];
    }
};
