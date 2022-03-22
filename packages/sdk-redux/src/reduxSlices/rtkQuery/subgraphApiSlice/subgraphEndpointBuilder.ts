import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

import {SubgraphBaseQuery} from './subgraphBaseQuery';
import {SubgraphReducerPath} from './subgraphReducerPath';

export type SubgraphEndpointBuilder = EndpointBuilder<SubgraphBaseQuery, CacheTagTypes, SubgraphReducerPath>;
