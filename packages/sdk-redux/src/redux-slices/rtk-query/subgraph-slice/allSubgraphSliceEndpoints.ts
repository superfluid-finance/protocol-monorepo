import {createEntityQueryEndpoints} from './createEntityQueryEndpoints';
import {createEventQueryEndpoints} from './createEventQueryEndpoints';
import {createCustomQueryEndpoints} from './customSubgraphQuery';
import {SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const allSubgraphSliceEndpoints = {
    endpoints: (builder: SubgraphSliceEndpointBuilder) => ({
        ...createCustomQueryEndpoints(builder),
        ...createEntityQueryEndpoints(builder),
        ...createEventQueryEndpoints(builder),
    }),
};
