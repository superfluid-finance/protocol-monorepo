import {insertIf} from '../../../utils';
import {NothingString} from '../../argTypes';

import {createTag} from './CacheTagTypes';

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
type IndexTagArg = {
    chainId: number;
    address1: string | NothingString;
    address2: string | NothingString;
    address3: string | NothingString;
    indexId: string | NothingString;
};

/**
 * Creates all possible index cache tag combinations.
 * @private
 * @category Cache Tags
 */
export const createIndexTags = ({chainId, address1, address2, address3, indexId}: IndexTagArg) => [
    indexTag(chainId),
    ...insertIf(address1, indexTag(chainId, address1!)),
    ...insertIf(address2, indexTag(chainId, address2!)),
    ...insertIf(address3, indexTag(chainId, address3!)),
    ...insertIf(address1 && address2, indexTag(chainId, address1!, address2!)),
    ...insertIf(address1 && address3, indexTag(chainId, address1!, address3!)),
    ...insertIf(address1 && address2 && address3, indexTag(chainId, address1!, address2!, address3!)),
    ...insertIf(
        address1 && address2 && address3 && indexId,
        indexTag(chainId, address1!, address2!, address3!, indexId!)
    ),
];

/**
 * @private
 * @category Cache Tags
 */
export const getMostSpecificIndexTag = (arg: IndexTagArg) => {
    return createIndexTags(arg).reverse()[0];
};

const indexTag = (chainId: number, ...keys: string[]) => createTag('Index', chainId, ...keys);
