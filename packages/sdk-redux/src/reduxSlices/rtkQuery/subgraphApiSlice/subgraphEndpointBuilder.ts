import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagType} from '../cacheTags/CacheTagTypes';

import {SubgraphBaseQuery} from './subgraphBaseQuery';
import {SubgraphReducerPath} from './subgraphReducerPath';

export type SubgraphEndpointBuilder = EndpointBuilder<SubgraphBaseQuery, CacheTagType, SubgraphReducerPath>;
