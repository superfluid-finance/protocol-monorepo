import {DocumentNode} from 'graphql';

import {BaseQuery} from '../../argTypes';

type RequestDocument = string | DocumentNode;

declare type Variables = {
    [key: string]: any;
};

export interface AdHocSubgraphQuery extends BaseQuery<unknown> {
    document: RequestDocument;
    variables?: Variables;
}
