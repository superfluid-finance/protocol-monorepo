// NOTE: If you add a type here then you have to also add it to tag types on the RTK Query slice.
/**
 * All possible cache tag types.
 * @private
 * @category Cache Tags
 */
export type CacheTagTypes = 'Event' | 'Index' | 'Stream' | 'Token';

/**
 * Create a cache tag for RTK-Query.
 * @private
 * @category Cache Tags
 */
export const createTag = (type: CacheTagTypes, ...keys: (string | number)[]) => ({
    type: type,
    id: keys.join('_').toLowerCase(),
});

// Order of address priorities if all present:
// 1. SuperToken Address
// 2. Publisher / Sender / From Address
// 3. Subscriber / Receiver / To Address
