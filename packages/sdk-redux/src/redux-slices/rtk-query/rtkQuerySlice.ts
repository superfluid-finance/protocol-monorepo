import { isPlainObject } from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';
import { typeGuard } from '../../utils';
import { CacheTagTypes } from './cacheTags/CacheTagTypes';

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
