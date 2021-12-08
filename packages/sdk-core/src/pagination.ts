import { ILightEntity } from "./interfaces";

/**
 * @dev Paging Helper Class
 */
export type Paging = {
    readonly take: number;
    readonly skip?: number;
    readonly lastId?: string;
};

/**
 * For paginating Subgraph queries by using skip and take approach. Good for small result sets, not performant for large sets.
 * Use {@link LastIdPaging} for most performant pagination strategy.
 */
export type SkipPaging = {
    readonly take: number;
    readonly skip: number;
};

/**
 * For paginating Subgraph queries by using the last ID of the previous paged result to get the next page.
 * Relies on Subgraph ordering ID's in ascending order which it always does unless the results are ordered by `id` in `desc` order.
 */
export type LastIdPaging = {
    readonly take: number;
    readonly lastId: string;
};

/**
 * @dev PagedResult Interface
 */
export interface PagedResult<T extends ILightEntity> {
    /**
     * The pagination used for current data.
     */
    readonly paging: Paging;
    /**
     * {@link Paging} for getting the next page.
     * `undefined` when there's no next page.
     */
    readonly nextPaging?: Paging;
    readonly data: T[];
}

/**
 * Factory function to create a {@link PagedResult}.
 * @param dataPlusOne Subgraph queries are executed with one extra result to get which is over the {@link Paging} `take` amount.
 * @param paging
 */
export const createPagedResult = <T extends ILightEntity>(
    dataPlusOne: T[],
    paging: Paging
): PagedResult<T> => {
    const hasNextPage = dataPlusOne.length > paging.take;
    const data = dataPlusOne.slice(0, paging.take);
    const lastId = data.slice(-1)[0]?.id;

    return {
        paging: { skip: paging.skip, take: paging.take },
        nextPaging: hasNextPage
            ? isSkipPaging(paging)
                ? nextSkipPaging(paging)
                : nextLastIdPaging(paging as LastIdPaging, lastId!)
            : undefined,
        data: data,
    };
};

function isSkipPaging(paging: Paging): paging is SkipPaging {
    return paging.skip !== undefined;
}

export const createSkipPaging = ({
    skip = 0,
    take = 100,
} = {}): SkipPaging => ({
    skip: skip,
    take: take,
});

export const createLastIdPaging = ({
    lastId = "",
    take = 100,
} = {}): LastIdPaging => ({
    take: take,
    lastId: lastId,
});

/**
 * @dev Gets the next page given the skip/take used to initialize the `PagedResult` interface.
 * @returns the `Paging` class with the next page
 */
export const nextSkipPaging = (paging: SkipPaging): SkipPaging => ({
    skip: paging.skip + paging.take,
    take: paging.take,
});

export const nextLastIdPaging = (
    paging: LastIdPaging,
    nextLastId: string
): LastIdPaging => ({
    take: paging.take,
    lastId: nextLastId,
});

/**
 * @dev Used to determine whether there is another page for pagination.
 * @returns the user's specified `take` plus one
 */
export const takePlusOne = (paging: Paging) => {
    return paging.take + 1;
};
