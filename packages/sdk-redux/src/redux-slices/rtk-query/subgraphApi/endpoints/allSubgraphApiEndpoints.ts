import SubgraphApiEndpointBuilder from '../subgraphApiEndpointBuilder';

import {createBaseEndpoints} from './baseEndpoints';
import {createEntityEndpoints} from './entityEndpoints';
import {createEventQueryEndpoints} from './eventEndpoints';

export const allSubgraphApiEndpoints = {
    endpoints: (builder: SubgraphApiEndpointBuilder) => ({
        ...createBaseEndpoints(builder),
        ...createEntityEndpoints(builder),
        ...createEventQueryEndpoints(builder),
    }),
};
