import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

import {SubgraphApiBaseQuery} from './subgraphApiBaseQuery';
import {SuperfluidApiReducerPath} from './subgraphApiReducerPath';

export type SubgraphApiEndpointBuilder = EndpointBuilder<SubgraphApiBaseQuery, CacheTagTypes, SuperfluidApiReducerPath>;
