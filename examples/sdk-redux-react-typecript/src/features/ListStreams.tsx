import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { IStream, useListStreamsQuery } from "@superfluid-finance/sdk-redux";
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

export const ListStreams: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);

    const [superTokenAddress, setSuperTokenAddress] = useState<string>("");
    const [senderAddress, setSenderAddress] = useState<string>("");
    const [receiverAddress, setReceiverAddress] = useState<string>("");

    useEffect(() => {
        setPage(1);
    }, [chainId, senderAddress, receiverAddress, superTokenAddress]);

    const {
        data: pagedStreams,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListStreamsQuery({
        chainId: chainId,
        senderAddress,
        receiverAddress,
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
                        label="SuperToken"
                        value={superTokenAddress}
                        onChange={(e) =>
                            setSuperTokenAddress(e.currentTarget.value)
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="Sender"
                        value={senderAddress}
                        onChange={(e) =>
                            setSenderAddress(e.currentTarget.value)
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="Receiver"
                        value={receiverAddress}
                        onChange={(e) =>
                            setReceiverAddress(e.currentTarget.value)
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
                                        <TableCell>Index</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    <TableBody>
                                        {pagedStreams!.data.map(
                                            (flow: IStream, index: number) => (
                                                <TableRow
                                                    key={index}
                                                    sx={{
                                                        "&:last-child td, &:last-child th":
                                                            {
                                                                border: 0,
                                                            },
                                                    }}
                                                >
                                                    <TableCell>
                                                        {chainId}
                                                    </TableCell>
                                                    <TableCell>
                                                        {flow.token.id}
                                                    </TableCell>
                                                    <TableCell>
                                                        {flow.sender.id}
                                                    </TableCell>
                                                    <TableCell>
                                                        {flow.receiver.id}
                                                    </TableCell>
                                                    <TableCell>
                                                        {flow.currentFlowRate}
                                                    </TableCell>
                                                </TableRow>
                                            )
                                        )}
                                    </TableBody>
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {pagedStreams && !error && (
                        <Pagination
                            count={pagedStreams.hasNextPage ? page + 1 : page}
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
