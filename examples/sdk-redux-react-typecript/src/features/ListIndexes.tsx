import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { IIndex } from "@superfluid-finance/sdk-core";
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

export const ListIndexes: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);

    const [indexId, setIndexId] = useState<string>("");
    const [publisherAddress, setPublisherAddress] =
        useState<string>(signerAddress);
    const [superTokenAddress, setSuperTokenAddress] = useState<string>("");

    useEffect(() => {
        setPage(1);
    }, [queryChainId, indexId, publisherAddress, superTokenAddress]);

    const {
        data: pagedIndexes,
        isFetching,
        isLoading,
        error,
        refetch,
    } = sfApi.useListIndexesQuery({
        chainId: queryChainId,
        indexId: indexId,
        publisherAddress,
        superTokenAddress,
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
                        label="Index ID"
                        value={indexId}
                        onChange={(e) => setIndexId(e.currentTarget.value)}
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="Publisher Address"
                        value={publisherAddress}
                        onChange={(e) =>
                            setPublisherAddress(e.currentTarget.value)
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
                                            createdAtTimestamp
                                        </TableCell>
                                        <TableCell>
                                            createdAtBlockNumber
                                        </TableCell>
                                        <TableCell>
                                            updatedAtTimestamp
                                        </TableCell>
                                        <TableCell>
                                            updatedAtBlockNumber
                                        </TableCell>
                                        <TableCell>indexId</TableCell>
                                        <TableCell>indexValue</TableCell>
                                        <TableCell>
                                            totalSubscriptionsWithUnits
                                        </TableCell>
                                        <TableCell>totalUnitsPending</TableCell>
                                        <TableCell>
                                            totalUnitsApproved
                                        </TableCell>
                                        <TableCell>totalUnits</TableCell>
                                        <TableCell>
                                            totalAmountDistributedUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>token</TableCell>
                                        <TableCell>publisher</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedIndexes!.data.map((index: IIndex) => (
                                        <TableRow key={index.id}>
                                            <TableCell>
                                                {index.createdAtTimestamp}
                                            </TableCell>
                                            <TableCell>
                                                {index.createdAtBlockNumber}
                                            </TableCell>
                                            <TableCell>
                                                {index.updatedAtTimestamp}
                                            </TableCell>
                                            <TableCell>
                                                {index.updatedAtBlockNumber}
                                            </TableCell>
                                            <TableCell>
                                                {index.indexId}
                                            </TableCell>
                                            <TableCell>
                                                {index.indexValue}
                                            </TableCell>
                                            <TableCell>
                                                {
                                                    index.totalSubscriptionsWithUnits
                                                }
                                            </TableCell>
                                            <TableCell>
                                                {index.totalUnitsPending}
                                            </TableCell>
                                            <TableCell>
                                                {index.totalUnitsApproved}
                                            </TableCell>
                                            <TableCell>
                                                {index.totalUnits}
                                            </TableCell>
                                            <TableCell>
                                                {
                                                    index.totalAmountDistributedUntilUpdatedAt
                                                }
                                            </TableCell>
                                            <TableCell>
                                                {index.token.name} (
                                                {index.token.id})
                                            </TableCell>
                                            <TableCell>
                                                {index.publisher}
                                            </TableCell>
                                        </TableRow>
                                    ))}
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {pagedIndexes && !error && (
                        <Pagination
                            count={pagedIndexes.nextPaging ? page + 1 : page}
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
