// NOTE: This might not include all the type info.
import {createApiSlice} from './sfApiSlice';

export type SfApiSliceInferredType = ReturnType<typeof createApiSlice>;
