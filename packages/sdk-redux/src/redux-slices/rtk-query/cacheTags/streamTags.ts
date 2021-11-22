import { NothingString } from '../../baseArg';
import { insertIf } from '../../../utils';
import { createTag } from './CacheTagTypes';

type StreamTagArg = {
    chainId: number;
    address1: string | NothingString;
    address2: string | NothingString;
    address3: string | NothingString;
};

export const createStreamsTags = ({
    chainId,
    address1,
    address2,
    address3,
}: StreamTagArg) => [
    streamTag(chainId),
    ...insertIf(address1, streamTag(chainId, address1!)),
    ...insertIf(address2, streamTag(chainId, address2!)),
    ...insertIf(address3, streamTag(chainId, address3!)),
    ...insertIf(address1 && address2, streamTag(chainId, address1!, address2!)),
    ...insertIf(address1 && address3, streamTag(chainId, address1!, address3!)),
    ...insertIf(
        address1 && address2 && address3,
        streamTag(chainId, address1!, address2!, address3!)
    ),
];

export const getMostSpecificStreamTag = (arg: StreamTagArg) => {
    return createStreamsTags(arg).reverse()[0];
};

const streamTag = (chainId: number, ...keys: string[]) =>
    createTag('Stream', chainId, ...keys);
