import { isPlainObject } from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';

export const rtkQuerySlice = createApi({
    reducerPath: 'superfluidApi',
    baseQuery: rtkQuerySliceBaseQuery(),
    tagTypes: ['Stream', 'Event'],
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
