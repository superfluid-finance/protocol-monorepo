import { TokenRender } from "../[name]";
import { sfApi, wrapper } from "../../../redux/store";

const queryArgs = {
    chainId: 5,
    take: 999,
    skip: 0,
    isListed: true,
};

export default function Token() {
    const result = sfApi.useListSuperTokensQuery(queryArgs);

    const { isFetching, error, data } = result;
    return TokenRender(isFetching, error, data);
}

export const getServerSideProps = wrapper.getServerSideProps(
    (store) => async (context) => {
        store.dispatch(sfApi.endpoints.listSuperTokens.initiate(queryArgs));

        await Promise.all(sfApi.util.getRunningOperationPromises());

        return {
            props: {},
        };
    }
);
