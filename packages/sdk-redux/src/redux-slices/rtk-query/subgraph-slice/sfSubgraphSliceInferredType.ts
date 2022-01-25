// NOTE: This might not include all the type info.
import {createSubgraphSlice} from './subgraphSlice';

export type SfSubgraphSliceInferredType = ReturnType<typeof createSubgraphSlice>;
