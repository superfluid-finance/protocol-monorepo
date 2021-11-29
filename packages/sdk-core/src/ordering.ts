export type OrderDirection =
    | 'asc'
    | 'desc';

export type Ordering<TOrderBy extends string> = {
    orderBy: TOrderBy,
    orderDirection: OrderDirection
};
