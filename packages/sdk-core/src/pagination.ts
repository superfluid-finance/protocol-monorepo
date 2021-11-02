export class Paging {
    readonly skip: number;
    readonly take: number;

    constructor({skip = 0, take = 100} = {}) {
        this.skip = skip;
        this.take = take;
    }

    // TODO(KK): Comment why.
    public takePlusOne() {
        return this.take + 1;
    }
}

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

    // TODO(KK) nextPage(): Paging
}
