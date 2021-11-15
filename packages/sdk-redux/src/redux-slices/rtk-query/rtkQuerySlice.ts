import { createApi } from '@reduxjs/toolkit/dist/query/react';
import { defaultSerializeQueryArgs } from '@reduxjs/toolkit/dist/query/defaultSerializeQueryArgs';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';

export const rtkQuerySlice = createApi({
    reducerPath: 'superfluidApi',
    baseQuery: rtkQuerySliceBaseQuery(),
    tagTypes: ['Stream', 'Event'],
    endpoints: () => ({}),
    serializeQueryArgs: ({ endpointDefinition, endpointName, queryArgs }) => {
        // TODO(KK): Normalize addresses here? Maybe also default skip/take.

        return defaultSerializeQueryArgs({
            endpointDefinition,
            endpointName,
            queryArgs,
        });
    },
});
