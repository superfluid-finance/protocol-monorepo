import { createTag } from './CacheTagTypes';

export const createEventTag = (chainId: number) =>
    createTag('Event', chainId);
