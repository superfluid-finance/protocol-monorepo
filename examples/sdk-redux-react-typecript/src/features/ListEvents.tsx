import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useState,
    useEffect,
} from "react";
import { AllEvents, useListEventsQuery } from "@superfluid-finance/sdk-redux";
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

const pageSize = 10;

export const ListEvents: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);
    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);

    useEffect(() => {
        setPage(1);
    }, [queryChainId, accountAddress]);

    const {
        data: pagedEvents,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListEventsQuery(
        {
            chainId: queryChainId,
            accountAddress,
            timestamp_gte: undefined,
            skip: (page - 1) * pageSize,
            take: pageSize,
        },
        {
            pollingInterval: 7500,
        }
    );

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
                        label="Address"
                        value={accountAddress}
                        onChange={(e) =>
                            setAccountAddress(e.currentTarget.value)
                        }
                    />
                </FormGroup>
            </form>
            {
                <>
                    {isLoading ? (
                        <Loader />
                    ) : error ? (
                        <Error error={error} retry={refetch} />
                    ) : (
                        <TableContainer>
                            <Table aria-label="simple table">
                                <TableHead>
                                    <TableRow>
                                        <TableCell>Event</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedEvents!.data.map(
                                        (event: AllEvents, index: number) => (
                                            <TableRow key={index}>
                                                <TableCell>
                                                    <pre>
                                                        {JSON.stringify(event)}
                                                    </pre>
                                                </TableCell>
                                            </TableRow>
                                        )
                                    )}
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {pagedEvents && !error && (
                        <Pagination
                            count={pagedEvents.hasNextPage ? page + 1 : page}
                            page={page}
                            onChange={(
                                event: React.ChangeEvent<unknown>,
                                value: number
                            ) => {
                                setPage(value);
                            }}
                        />
                    )}
                </>
            }
        </>
    );
};
