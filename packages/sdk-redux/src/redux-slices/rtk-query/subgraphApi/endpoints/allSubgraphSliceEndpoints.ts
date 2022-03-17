import SubgraphApiEndpointBuilder from '../subgraphApiEndpointBuilder';

import {createEntityQueryEndpoints} from './createEntityQueryEndpoints';
import {createEventQueryEndpoints} from './createEventQueryEndpoints';
import {createCustomQueryEndpoints} from './customSubgraphQuery';

export const allSubgraphSliceEndpoints = {
    endpoints: (builder: SubgraphApiEndpointBuilder) => ({
        ...createCustomQueryEndpoints(builder),
        ...createEntityQueryEndpoints(builder),
        ...createEventQueryEndpoints(builder),
    }),
};
