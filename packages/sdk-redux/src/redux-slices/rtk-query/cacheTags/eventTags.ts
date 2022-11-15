import {createTag} from './CacheTagTypes';

/**
 * The simplest cache tag.
 * @private
 * @category Cache Tags
 */
export const createEventTag = (chainId: number) => createTag('Event', chainId);
