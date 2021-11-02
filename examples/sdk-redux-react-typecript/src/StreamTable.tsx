import React, { FC, ReactElement, useContext } from "react";
import { IStream, useFetchFlowsQuery } from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import {
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
} from "@mui/material";
import { SignerContext } from "./SignerContext";

export const StreamTable: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const { data: flows, isLoading } = useFetchFlowsQuery({
        chainId,
        accountAddress: signerAddress,
    });

    return (
        <>
            {isLoading ? (
                <Loader />
            ) : (
                <TableContainer>
                    <Table aria-label="simple table">
                        <TableHead>
                            <TableRow>
                                <TableCell>Chain</TableCell>
                                <TableCell>SuperToken</TableCell>
                                <TableCell>Sender</TableCell>
                                <TableCell>Receiver</TableCell>
                                <TableCell>Flow Rate</TableCell>
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {flows!.map((flow: IStream, index) => (
                                <TableRow
                                    key={index}
                                    sx={{
                                        "&:last-child td, &:last-child th": {
                                            border: 0,
                                        },
                                    }}
                                >
                                    <TableCell>{chainId}</TableCell>
                                    <TableCell>{flow.token.id}</TableCell>
                                    <TableCell>{flow.sender.id}</TableCell>
                                    <TableCell>{flow.receiver.id}</TableCell>
                                    <TableCell>{flow.currentFlowRate}</TableCell>
                                </TableRow>
                            ))}
                        </TableBody>
                    </Table>
                </TableContainer>
            )}
        </>
    );
};
