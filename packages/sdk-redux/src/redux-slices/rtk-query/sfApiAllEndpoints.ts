import {addMutationEndpoints} from './mutations/addMutationEndpoints';
import {addQueryEndpoints} from './queries/addQueryEndpoints';
import {ApiSliceEndpointBuilder} from './sfApiSlice';

export const sfApiAllEndpoints = {
    endpoints: (builder: ApiSliceEndpointBuilder) => ({
        ...addQueryEndpoints(builder),
        ...addMutationEndpoints(builder),
    }),
};

export default sfApiAllEndpoints;
