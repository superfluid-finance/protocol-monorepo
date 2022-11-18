import { TokenRender } from "../[name]";
import { subgraphApi, wrapper } from "../../../redux/store";
import { useRouter } from "next/router";

const queryArgs = {
    chainId: 5,
    filter: {
        isListed: true,
    },
    pagination: {
        take: Infinity,
    },
};

export default function TokenPage() {
    const router = useRouter();
    const result = subgraphApi.useTokensQuery(queryArgs, {
        // If the page is not yet generated, router.isFallback will be true
        // initially until getStaticProps() finishes running
        skip: router.isFallback,
    });

    const { isFetching, error, data } = result;
    return TokenRender(isFetching, error, data);
}

export const getStaticProps = wrapper.getStaticProps(
    (store) => async (context) => {
        store.dispatch(subgraphApi.endpoints.tokens.initiate(queryArgs));

        await Promise.all(store.dispatch(subgraphApi.util.getRunningQueriesThunk()));

        return {
            props: {},
        };
    }
);

export async function getStaticPaths() {
    return {
        paths: ["/tokens/ssg/fdaix"],
        fallback: true,
    };
}
