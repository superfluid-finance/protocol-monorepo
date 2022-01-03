import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    ILightAccountTokenSnapshot,
} from "@superfluid-finance/sdk-core";
import { Loader } from "../Loader";
import {
    FormGroup,
    Pagination,
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
    TextField,
} from "@mui/material";
import { SignerContext } from "../SignerContext";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

const pageSize = 10;

export const ListUserInteractedSuperTokens: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);

    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);
    const [superTokenAddress, setSuperTokenAddress] = useState("");

    useEffect(() => {
        setPage(1);
    }, [queryChainId, superTokenAddress, accountAddress]);

    const {
        data: pagedIndexSubscriptions,
        isFetching,
        isLoading,
        error,
        refetch,
    } = sfApi.useListUserInteractedSuperTokensQuery({
        accountAddress,
        superTokenAddress,
        chainId: queryChainId,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <TextField
                        sx={{ m: 1 }}
                        label="Chain ID"
                        value={queryChainId}
                        onChange={(e) =>
                            setQueryChainId(Number(e.currentTarget.value))
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        value={accountAddress}
                        label="Account Address"
                        onChange={(e) =>
                            setAccountAddress(e.currentTarget.value)
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="SuperToken Address"
                        value={superTokenAddress}
                        onChange={(e) =>
                            setSuperTokenAddress(e.currentTarget.value)
                        }
                    />
                </FormGroup>
            </form>
            {
                <>
                    {isFetching ? (
                        <Loader />
                    ) : error ? (
                        <Error error={error} retry={refetch} />
                    ) : (
                        <TableContainer>
                            <Table aria-label="simple table">
                                <TableHead>
                                    <TableRow>
                                        <TableCell>
                                            updatedAtTimestamp
                                        </TableCell>
                                        <TableCell>
                                            updatedAtBlockNumber
                                        </TableCell>
                                        <TableCell>
                                            totalNumberOfActiveStreams
                                        </TableCell>
                                        <TableCell>
                                            totalNumberOfClosedStreams
                                        </TableCell>
                                        <TableCell>
                                            totalSubscriptionsWithUnits
                                        </TableCell>
                                        <TableCell>
                                            totalApprovedSubscriptions
                                        </TableCell>
                                        <TableCell>
                                            balanceUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>totalNetFlowRate</TableCell>
                                        <TableCell>totalInflowRate</TableCell>
                                        <TableCell>totalOutflowRate</TableCell>
                                        <TableCell>
                                            totalAmountStreamedUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>
                                            totalAmountTransferredUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>account</TableCell>
                                        <TableCell>token</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedIndexSubscriptions!.data.map(
                                        (
                                            tokenSnapshot: ILightAccountTokenSnapshot
                                        ) => (
                                            <TableRow key={tokenSnapshot.id}>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.updatedAtTimestamp
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.updatedAtBlockNumber
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalNumberOfActiveStreams
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalNumberOfClosedStreams
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalSubscriptionsWithUnits
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalApprovedSubscriptions
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.balanceUntilUpdatedAt
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalNetFlowRate
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalInflowRate
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalOutflowRate
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalAmountStreamedUntilUpdatedAt
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        tokenSnapshot.totalAmountTransferredUntilUpdatedAt
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {tokenSnapshot.account}
                                                </TableCell>
                                                <TableCell>
                                                    {tokenSnapshot.token.name} (
                                                    {tokenSnapshot.token.id})
                                                </TableCell>
                                            </TableRow>
                                        )
                                    )}
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {pagedIndexSubscriptions && !error && (
                        <Pagination
                            count={
                                pagedIndexSubscriptions.nextPaging
                                    ? page + 1
                                    : page
                            }
                            onChange={(
                                event: React.ChangeEvent<unknown>,
                                value: number
                            ) => setPage(value)}
                        />
                    )}
                </>
            }
        </>
    );
};
