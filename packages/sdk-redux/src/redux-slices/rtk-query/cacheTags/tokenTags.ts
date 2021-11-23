import { insertIf } from '../../../utils';
import { NothingString } from '../../argTypes';
import { createTag } from './CacheTagTypes';

type TokenTagArg = {
    chainId: number;
    address1: string | NothingString;
    address2: string | NothingString;
    address3: string | NothingString;
};

export const createTokenTags = ({
    chainId,
    address1,
    address2,
    address3,
}: TokenTagArg) => [
    tokenTag(chainId),
    ...insertIf(address1, tokenTag(chainId, address1!)),
    ...insertIf(address2, tokenTag(chainId, address2!)),
    ...insertIf(address3, tokenTag(chainId, address3!)),
    ...insertIf(address1 && address3, tokenTag(chainId, address1!, address3!)),
    ...insertIf(address1 && address2, tokenTag(chainId, address1!, address2!)),
    ...insertIf(
        address1 && address2 && address3,
        tokenTag(chainId, address1!, address2!, address3!)
    ),
];

export const getMostSpecificTokenTag = (arg: TokenTagArg) => {
    return createTokenTags(arg).reverse()[0];
};

const tokenTag = (chainId: number, ...keys: string[]) =>
    createTag('Token', chainId, ...keys);
