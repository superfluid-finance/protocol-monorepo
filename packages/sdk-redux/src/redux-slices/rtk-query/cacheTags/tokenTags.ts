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
type TokenTagArg = {
    chainId: number;
    address1: string | NothingString;
    address2: string | NothingString;
    address3: string | NothingString;
};

/**
 * Creates all possible token cache tag combinations.
 * @private
 * @category Cache Tags
 */
export const createTokenTags = ({chainId, address1, address2, address3}: TokenTagArg) => [
    tokenTag(chainId),
    ...insertIf(address1, tokenTag(chainId, address1!)),
    ...insertIf(address2, tokenTag(chainId, address2!)),
    ...insertIf(address3, tokenTag(chainId, address3!)),
    ...insertIf(address1 && address3, tokenTag(chainId, address1!, address3!)),
    ...insertIf(address1 && address2, tokenTag(chainId, address1!, address2!)),
    ...insertIf(address1 && address2 && address3, tokenTag(chainId, address1!, address2!, address3!)),
];

/**
 * @private
 * @category Cache Tags
 */
export const getMostSpecificTokenTag = (arg: TokenTagArg) => {
    return createTokenTags(arg).reverse()[0];
};

const tokenTag = (chainId: number, ...keys: string[]) => createTag('Token', chainId, ...keys);
