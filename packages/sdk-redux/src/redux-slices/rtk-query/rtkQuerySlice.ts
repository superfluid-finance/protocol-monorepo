import { isPlainObject } from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';
import { typeGuard } from '../../utils';

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

export const eventTag = (chainId: number, ...keys: string[]) => ({
    type: typeGuard<TagTypes>('Event'),
    id: `${chainId}_${keys.join('_')}`.toLowerCase(),
});

export const indexTag = (chainId: number, ...keys: string[]) => ({
    type: typeGuard<TagTypes>('Index'),
    id: `${chainId}_${keys.join('_')}`.toLowerCase(),
});

export const streamTag = (chainId: number, ...keys: string[]) => ({
    type: typeGuard<TagTypes>('Stream'),
    id: `${chainId}_${keys.join('_')}`.toLowerCase(),
});

export const tokenTag = (chainId: number, ...keys: string[]) => ({
    type: typeGuard<TagTypes>('Token'),
    id: `${chainId}_${keys.join('_')}`.toLowerCase(),
});

export const accountTag = (chainId: number, ...keys: string[]) => ({
    type: typeGuard<TagTypes>('Account'),
    id: `${chainId}_${keys.join('_')}`.toLowerCase(),
});


// export const indexTags = (chainId: number, ...keys: string[]) => ({
//     type: typeGuard<TagTypes>('Index'),
//     id: `${chainId}_${keys.join('_')}`.toLowerCase(),
// });
