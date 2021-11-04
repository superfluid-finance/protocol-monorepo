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
 * @dev PagedResult Helper Class
 */
export class PagedResult<T> {
    readonly hasNextPage: boolean;
    readonly skip: number;
    readonly take: number;
    readonly data: T[];

    constructor(dataPlusOne: T[], paging: Paging) {
        this.take = paging.take;
        this.skip = paging.skip;
        this.hasNextPage = dataPlusOne.length > paging.take;
        this.data = dataPlusOne.slice(0, paging.take);
    }

    /**
     * @dev Gets the next page given the skip/take used to initialize the `PagedResult` class.
     * @returns the `Paging` class with the next page
     */
    public nextPage(): Paging {
        return new Paging({
            skip: this.take,
            take: this.take + (this.take - this.skip),
        });
    }
}
