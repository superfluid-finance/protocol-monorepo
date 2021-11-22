import { isPlainObject } from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';
import { insertIf, typeGuard } from '../../utils';
import { NothingString } from '../baseArg';

// NOTE: New tag types have to be added to the API slice.
export type TagTypes = 'Event' | 'Index' | 'Stream' | 'Token' | 'Account';

export const rtkQuerySlice = createApi({
    reducerPath: 'superfluidApi',
    baseQuery: rtkQuerySliceBaseQuery(),
    tagTypes: [
        typeGuard<TagTypes>('Event'),
        typeGuard<TagTypes>('Index'),
        typeGuard<TagTypes>('Stream'),
        typeGuard<TagTypes>('Token'),
        typeGuard<TagTypes>('Account'),
    ],
    endpoints: () => ({}),
    serializeQueryArgs: ({ endpointName, queryArgs }) => {
        // TODO(KK): Normalize addresses here? Maybe also default skip/take.

        // NOTE: The code below is taken from Redux Toolkit's repository from `defaultSerializeQueryArgs.ts`.

        // Sort the object keys before stringifying, to prevent useQuery({ a: 1, b: 2 }) having a different cache key than useQuery({ b: 2, a: 1 })
        return `${endpointName}(${JSON.stringify(queryArgs, (_key, value) =>
            isPlainObject(value)
                ? Object.keys(value)
                      .sort()
                      .reduce<any>((acc, key) => {
                          acc[key] = (value as any)[key];
                          return acc;
                      }, {})
                : value
        )})`;
    },
});

const indexTag = (chainId: number, ...keys: string[]) =>
    tag('Index', chainId, ...keys);

const streamTag = (chainId: number, ...keys: string[]) =>
    tag('Stream', chainId, ...keys);

const tokenTag = (chainId: number, ...keys: string[]) =>
    tag('Token', chainId, ...keys);

const tag = (type: TagTypes, ...keys: (string | number)[]) => ({
    type: type,
    id: keys.join('_').toLowerCase(),
});

// Order of address priorities if all present:
// 1. SuperToken Address
// 2. Publisher / Sender / From Address
// 3. Subscriber / Receiver / To Address

type IndexTagArg = {
    chainId: number;
    address1: string | NothingString;
    address2: string | NothingString;
    address3: string | NothingString;
    indexId: string | NothingString;
};

export const createIndexTags = ({
    chainId,
    address1,
    address2,
    address3,
    indexId,
}: IndexTagArg) => [
    indexTag(chainId),
    ...insertIf(address1, indexTag(chainId, address1!)),
    ...insertIf(address2, indexTag(chainId, address2!)),
    ...insertIf(address3, indexTag(chainId, address3!)),
    ...insertIf(address1 && address2, indexTag(chainId, address1!, address2!)),
    ...insertIf(address1 && address3, indexTag(chainId, address1!, address3!)),
    ...insertIf(
        address1 && address2 && address3,
        indexTag(chainId, address1!, address2!, address3!)
    ),
    ...insertIf(
        address1 && address2 && address3 && indexId,
        indexTag(chainId, address1!, address2!, address3!, indexId!)
    ),
];

export const getMostSpecificIndexTag = (arg: IndexTagArg) => {
    return createIndexTags(arg).reverse()[0];
};

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
    indexTag(chainId),
    ...insertIf(address1, streamTag(chainId, address1!)),
    ...insertIf(address2, streamTag(chainId, address2!)),
    ...insertIf(address3, streamTag(chainId, address3!)),
    ...insertIf(
        address1 && address2,
        streamTag(chainId, address1!, address2!)
    ),
    ...insertIf(
        address1 && address3,
        streamTag(chainId, address1!, address3!)
    ),
    ...insertIf(
        address1 && address2 && address3,
        streamTag(chainId, address1!, address2!, address3!)
    ),
];

export const getMostSpecificStreamTag = (arg: StreamTagArg) => {
    return createStreamsTags(arg).reverse()[0];
};

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
    ...insertIf(
        address1 && address3,
        tokenTag(chainId, address1!, address3!)
    ),
    ...insertIf(
        address1 && address2,
        tokenTag(chainId, address1!, address2!)
    ),
    ...insertIf(
        address1 && address2 && address3,
        tokenTag(chainId, address1!, address2!, address3!)
    ),
];

export const getMostSpecificTokenTag = (arg: TokenTagArg) => {
    return createTokenTags(arg).reverse()[0];
};
