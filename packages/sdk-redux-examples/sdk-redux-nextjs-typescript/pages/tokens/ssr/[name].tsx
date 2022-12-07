import { TokenRender } from "../[name]";
import { subgraphApi, wrapper } from "../../../redux/store";

const queryArgs = {
    chainId: 5,
    filter: {
        isListed: true
    },
    pagination: {
        take: Infinity
    }
};

export default function TokenPage() {
    const result = subgraphApi.useTokensQuery(queryArgs);

    const { isFetching, error, data } = result;
    return TokenRender(isFetching, error, data);
}

export const getServerSideProps = wrapper.getServerSideProps(
    (store) => async (context) => {
        store.dispatch(subgraphApi.endpoints.tokens.initiate(queryArgs));

        await Promise.all(store.dispatch(subgraphApi.util.getRunningQueriesThunk()));

        return {
            props: {},
        };
    }
);
