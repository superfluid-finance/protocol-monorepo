import { createApi } from '@reduxjs/toolkit/dist/query/react';

import { rtkQuerySliceBaseQuery } from './rtkQuerySliceBaseQuery';

export const rtkQuerySlice = createApi({
    reducerPath: 'superfluidApi',
    baseQuery: rtkQuerySliceBaseQuery(),
    tagTypes: ['Flow'],
    endpoints: () => ({}),
});
