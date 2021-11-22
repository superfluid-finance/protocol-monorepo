// NOTE: If you add a type here then you have to also add it to tag types on the RTK Query slice.
export type CacheTagTypes = 'Index' | 'Stream' | 'Token';

export const createTag = (type: CacheTagTypes, ...keys: (string | number)[]) => ({
    type: type,
    id: keys.join('_').toLowerCase(),
});

// Order of address priorities if all present:
// 1. SuperToken Address
// 2. Publisher / Sender / From Address
// 3. Subscriber / Receiver / To Address
