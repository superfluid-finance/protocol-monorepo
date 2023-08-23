import {useRouter} from "next/router";
import Layout from "../../components/layout";
import Head from "next/head";
import {SerializedError} from "@reduxjs/toolkit";
import { subgraphApi, useAppDispatch } from "../../redux/store";
import {useState} from "react";
import { PagedResult, Token } from "@superfluid-finance/sdk-core";

export default function TokenPage() {
    const dispatch = useAppDispatch();

    const [shouldResetApi, setShouldResetApi] = useState(true);

    if (shouldResetApi) {
        dispatch(subgraphApi.util.resetApiState());
        setShouldResetApi(false);
    }

    const result = subgraphApi.useTokensQuery({
        chainId: 5,
        filter: {
            isListed: true
        },
        pagination: {
            take: Infinity
        }
    });

    const {isFetching, error, data} = result;
    return TokenRender(isFetching, error, data);
}

export function TokenRender(
    isLoading: boolean,
    error: SerializedError | undefined,
    data: PagedResult<Token> | undefined
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
