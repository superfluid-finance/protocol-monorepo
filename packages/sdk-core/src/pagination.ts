/**
 * @dev Paging Helper Class
 */
export class Paging {
    readonly skip: number;
    readonly take: number;

    constructor({ skip = 0, take = 100 } = {}) {
        this.skip = skip;
        this.take = take;
    }

    /**
     * @dev Used to determine whether there is another page for pagination.
     * @returns the user's specified `take` plus one
     */
    public takePlusOne() {
        return this.take + 1;
    }
}

/**
 * @dev PagedResult Interface
 */
export interface PagedResult<T> {
    readonly hasNextPage: boolean;
    readonly skip: number;
    readonly take: number;
    readonly data: T[];
}

export const createPagedResult = <T>(
    dataPlusOne: T[],
    paging: Paging
): PagedResult<T> => ({
    take: paging.take,
    skip: paging.skip,
    hasNextPage: dataPlusOne.length > paging.take,
    data: dataPlusOne.slice(0, paging.take),
});

/**
 * @dev Gets the next page given the skip/take used to initialize the `PagedResult` interface.
 * @returns the `Paging` class with the next page
 */
export const nextPage = (paging: Paging) =>
    new Paging({
        skip: paging.take,
        take: paging.take + (paging.take - paging.skip),
    });
