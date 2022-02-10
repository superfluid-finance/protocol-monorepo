import {useRouter} from "next/router";
import Layout from "../../components/layout";
import Head from "next/head";
import {SerializedError} from "@reduxjs/toolkit";
import { sfApi, useAppDispatch } from "../../redux/store";
import {useState} from "react";
import {
    ValidationError
} from "@superfluid-finance/sdk-redux";
import { ISuperToken, PagedResult } from "@superfluid-finance/sdk-core";

export default function Token() {
    const dispatch = useAppDispatch();

    const [shouldResetApi, setShouldResetApi] = useState(true);

    if (shouldResetApi) {
        dispatch(sfApi.util.resetApiState());
        setShouldResetApi(false);
    }

    const result = sfApi.useListSuperTokensQuery({
        chainId: 5,
        take: 999,
        skip: 0,
        isListed: true,
    });

    const {isFetching, error, data} = result;
    return TokenRender(isFetching, error, data);
}

export function TokenRender(
    isLoading: boolean,
    error: ValidationError | SerializedError | undefined,
    data: PagedResult<ISuperToken> | undefined
) {
    const router = useRouter();
    const name = router.query.name;
    const token = data?.data.filter((x) => x.symbol.toLowerCase() === name)[0];

    return (
        <Layout>
            <Head>
                <title>{token?.id ?? ""}</title>
            </Head>
            <article>
                {error ? (
                    <>Oh no, there was an error</>
                ) : router.isFallback || isLoading ? (
                    <>Loading...</>
                ) : token ? (
                    <>
                        <h3>{token.symbol}</h3>
                        <dl>
                            <dt>Address</dt>
                            <dd>{token.id}</dd>
                            <dt>Created at</dt>
                            <dd>
                                {new Date(
                                    token.createdAtTimestamp * 1000
                                ).toString()}
                            </dd>
                            <dt>Underlying address</dt>
                            <dd>{token.underlyingAddress}</dd>
                        </dl>
                    </>
                ) : (
                    <>Token not found.</>
                )}
            </article>
        </Layout>
    );
}
