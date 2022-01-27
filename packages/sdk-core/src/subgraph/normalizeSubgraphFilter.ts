export const normalizeSubgraphFilter = (value: object) => {
    return Object.keys(value)
        .sort()
        .reduce<any>((acc, key) => {
            acc[key] = normalizeSubgraphFilterValue((value as any)[key]);
            return acc;
        }, {});
};

/**
 * NOTE: Regex taken from Ethers.
 */
const isAddressRegex = /^(0x)?[0-9a-fA-F]{40}$/;

/**
 * Normalize addresses and empty strings for cache keys.
 */
export const normalizeSubgraphFilterValue = (value: unknown) =>
    lowerCaseIfAddress(undefinedIfEmpty(value));

const undefinedIfEmpty = (value: unknown) => {
    if (value === "") {
        return undefined;
    }
    return value;
};

const lowerCaseIfAddress = (value: unknown): unknown => {
    if (typeof value === "string") {
        if (value.match(isAddressRegex)) {
            return value.toLowerCase();
        }
    }

    if (Array.isArray(value)) {
        return value.map((x) => lowerCaseIfAddress(x));
    }

    return value;
};
