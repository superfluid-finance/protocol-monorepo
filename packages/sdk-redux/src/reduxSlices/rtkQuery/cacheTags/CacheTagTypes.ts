import {insertIf, typeGuard} from '../../../utils';
import {NothingString} from '../../argTypes';

/**
 * All possible cache tag types.
 *
 * @private
 * @category Cache Tags
 */
export type CacheTagType = 'Event' | 'TokenList' | 'Index' | 'Stream' | 'Balance'; // NOTE: If you add a type here then you have to also add it to tag types on the RTK Query slices.

export const cacheTagTypes = typeGuard<CacheTagType[]>(['Event', 'TokenList', 'Index', 'Stream', 'Balance']);

/**
 * Create a cache tag for RTK-Query.
 * @private
 * @category Cache Tags
 */
export const createTag = (type: CacheTagType, ...keys: (string | number | undefined)[]) => ({
    type: type,
    id: keys
        .filter((x) => x !== undefined)
        .join('_')
        .toLowerCase(),
});

/**
 * NOTE:
 * Always order addresses in the following priority (if all addresses present):
 * * 1. SuperToken Address
 * * 2. Publisher / Sender / From Address
 * * 3. Subscriber / Receiver / To Address
 * If an address is not present then the rest of the addresses move up in priority.
 * @private
 * @category Cache Tags
 */
type CacheTagPayload = {
    chainId: number;
    address1?: string | NothingString;
    address2?: string | NothingString;
    address3?: string | NothingString;
};

export const createTags = (
    specificTagTypes: Exclude<CacheTagType, 'Event'>[],
    {chainId, address1, address2, address3}: CacheTagPayload
): ReturnType<typeof createTag>[] =>
    [
        createTag('Event', chainId),
        ...insertIf(address1, () => [createTag('Event', chainId, address1!)]),
        ...insertIf(address2, () => [createTag('Event', chainId, address2!)]),
        ...insertIf(address3, () => [createTag('Event', chainId, address3!)]),
    ].concat(
        specificTagTypes
            .map((tagType) => [
                createTag(tagType, chainId),
                ...insertIf(address1, () => [createTag(tagType, chainId, address1!)]),
                ...insertIf(address2, () => [
                    createTag(tagType, chainId, address2!),
                    createTag(tagType, chainId, address1!, address2!),
                ]),
                ...insertIf(address3, () => [
                    createTag(tagType, chainId, address3!),
                    createTag(tagType, chainId, address1!, address3!),
                    createTag(tagType, chainId, address1!, address2!, address3!),
                ]),
            ])
            .flat()
    );
