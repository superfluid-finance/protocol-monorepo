import { ILightEntity } from "./interfaces";

export type OrderDirection = "asc" | "desc";

export type Ordering<TOrderBy extends string> = {
    by: TOrderBy;
    direction: OrderDirection;
};

/**
 * @dev Paging Helper Class
 */
export type Paging = {
    readonly take: number;
    readonly skip?: number;
    readonly lastId?: string;
};

export type SkipPaging = {
    readonly take: number;
    readonly skip: number;
};

export type LastIdPaging = {
    readonly take: number;
    readonly lastId: string;
};

/**
 * @dev PagedResult Interface
 */
export interface PagedResult<T extends ILightEntity> {
    readonly currentPaging: Paging;
    readonly hasNextPage: boolean;
    readonly nextPaging?: Paging;
    readonly data: T[];
}

export const createPagedResult = <T extends ILightEntity>(
    dataPlusOne: T[],
    paging: Paging
): PagedResult<T> => {
    const hasNextPage = dataPlusOne.length > paging.take;
    const data = dataPlusOne.slice(0, paging.take);
    const lastId = data.slice(-1)[0]?.id;

    return {
        currentPaging: { skip: paging.skip, take: paging.take },
        hasNextPage: hasNextPage,
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

export const createSkipPaging = ({ skip = 0, take = 100 } = {}): SkipPaging => ({
    skip: skip,
    take: take
});

export const createLastIdPaging = ({ lastId = "", take = 100 } = {}): LastIdPaging => ({
    take: take,
    lastId: lastId
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
//
// export const getLastId = (paging: Paging): string =>
//     typeof paging.skip === "number" ? "" : paging.skip;
// export const getSkip = (paging: Paging): number =>
//     typeof paging.skip === "number" ? paging.skip : 0;
