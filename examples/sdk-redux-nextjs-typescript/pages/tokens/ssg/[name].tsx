import { TokenRender } from "../[name]";
import { makeStore, sfApi, wrapper } from "../../../redux/store";
import { useRouter } from "next/router";

const queryArgs = {
    chainId: 5,
    take: 999,
    skip: 0,
    isListed: true,
};

export default function Token() {
    const router = useRouter();
    const result = sfApi.useListSuperTokensQuery(queryArgs, {
        // If the page is not yet generated, router.isFallback will be true
        // initially until getStaticProps() finishes running
        skip: router.isFallback,
    });

    const { isFetching, error, data } = result;
    return TokenRender(isFetching, error, data);
}

export const getStaticProps = wrapper.getStaticProps(
    (store) => async (context) => {
        store.dispatch(sfApi.endpoints.listSuperTokens.initiate(queryArgs));

        await Promise.all(sfApi.util.getRunningOperationPromises());

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
