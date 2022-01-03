import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { IStream } from "@superfluid-finance/sdk-core";
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
import { FlowingBalance } from "../FlowingBalance";
import { ethers } from "ethers";
import { sfApi } from "../redux/store";

const pageSize = 10;

export const ListStreams: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);

    const [superTokenAddress, setSuperTokenAddress] = useState<string>("");
    const [senderAddress, setSenderAddress] = useState<string>("");
    const [receiverAddress, setReceiverAddress] = useState<string>("");

    useEffect(() => {
        setPage(1);
    }, [queryChainId, senderAddress, receiverAddress, superTokenAddress]);

    const {
        data: pagedStreams,
        isFetching,
        isLoading,
        error,
        refetch,
    } = sfApi.useListStreamsQuery({
        chainId: queryChainId,
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
                        label="Chain ID"
                        value={queryChainId}
                        onChange={(e) =>
                            setQueryChainId(Number(e.currentTarget.value))
                        }
                    />
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
                                        <TableCell>SuperToken</TableCell>
                                        <TableCell>Sender</TableCell>
                                        <TableCell>Receiver</TableCell>
                                        <TableCell>Flow Rate</TableCell>
                                        <TableCell>Total Streamed</TableCell>
                                    </TableRow>
                                </TableHead>
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
                                                    {flow.token.id}
                                                </TableCell>
                                                <TableCell>
                                                    {flow.sender}
                                                </TableCell>
                                                <TableCell>
                                                    {flow.receiver}
                                                </TableCell>
                                                <TableCell>
                                                    {flow.currentFlowRate}
                                                </TableCell>
                                                <TableCell>
                                                    <FlowingBalance
                                                        format={(x) =>
                                                            ethers.utils.formatUnits(
                                                                x
                                                            )
                                                        }
                                                        balanceWei={
                                                            flow.streamedUntilUpdatedAt
                                                        }
                                                        balanceTimestamp={
                                                            flow.updatedAtTimestamp
                                                        }
                                                        flowRateWei={
                                                            flow.currentFlowRate
                                                        }
                                                    />
                                                </TableCell>
                                            </TableRow>
                                        )
                                    )}
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {pagedStreams && !error && (
                        <Pagination
                            count={pagedStreams.nextPaging ? page + 1 : page}
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
