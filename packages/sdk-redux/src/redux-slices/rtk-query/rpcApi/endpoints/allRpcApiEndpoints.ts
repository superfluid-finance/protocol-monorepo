import RpcApiEndpointBuilder from '../rpcApiEndpointBuilder';

import {createFlowEndpoints} from './flowEndpoints';
import {createIndexEndpoints} from './indexEndpoints';
import {createSuperTokenEndpoints} from './superTokenEndpoints';

const allRpcApiEndpoints = {
    endpoints: (builder: RpcApiEndpointBuilder) =>
        Object.assign(createFlowEndpoints(builder), createIndexEndpoints(builder), createSuperTokenEndpoints(builder)),
};

export default allRpcApiEndpoints;
