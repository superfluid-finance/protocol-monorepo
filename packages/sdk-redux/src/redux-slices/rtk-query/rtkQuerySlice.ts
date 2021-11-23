import { isPlainObject } from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';
import { typeGuard } from '../../utils';
import { CacheTagTypes } from './cacheTags/CacheTagTypes';
import _ from 'lodash';

export const rtkQuerySlice = createApi({
    reducerPath: 'sfApi',
    baseQuery: rtkQuerySliceBaseQuery(),
    tagTypes: [
        typeGuard<CacheTagTypes>('Event'),
        typeGuard<CacheTagTypes>('Index'),
        typeGuard<CacheTagTypes>('Stream'),
        typeGuard<CacheTagTypes>('Token'),
    ],
    endpoints: () => ({}),
    serializeQueryArgs: ({ endpointName, queryArgs }) => {
        // NOTE: The code below is taken from Redux Toolkit's repository from `defaultSerializeQueryArgs.ts`.

        // Comment from RTK-Query: Sort the object keys before stringifying, to prevent useQuery({ a: 1, b: 2 }) having a different cache key than useQuery({ b: 2, a: 1 })
        return `${endpointName}(${JSON.stringify(queryArgs, (_key, value) =>
            isPlainObject(value)
                ? Object.keys(value)
                      .sort()
                      .reduce<any>((acc, key) => {
                          acc[key] = normalizeValue((value as any)[key]);
                          return acc;
                      }, {})
                : normalizeValue(value)
        )})`;
    },
});

export type SuperfluidApiReduxSliceType = typeof rtkQuerySlice;

// NOTE: Regex taken from Ethers.
const isAddressRegex = /^(0x)?[0-9a-fA-F]{40}$/;

// Normalize addresses and empty strings for cache keys.
const normalizeValue = (value: unknown) =>
    lowerCaseIfAddress(undefinedIfEmpty(value));

const undefinedIfEmpty = (value: unknown) => {
    if (value === '') {
        return undefined;
    }
    return value;
};

const lowerCaseIfAddress = (value: unknown) => {
    if (_.isString(value)) {
        if (value.match(isAddressRegex)) {
            return value.toLowerCase();
        }
    }
    return value;
};
