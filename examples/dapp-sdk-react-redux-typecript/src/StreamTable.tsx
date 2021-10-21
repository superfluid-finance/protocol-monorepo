import React, { FC, ReactElement, useContext } from "react";
import { Flow, useFetchFlowsQuery } from "dapp-sdk";
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
    const [networkName, signerAddress] = useContext(SignerContext);
    const { data: flows, isLoading } = useFetchFlowsQuery({
        networkName,
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
                                <TableCell>Network</TableCell>
                                <TableCell>SuperToken</TableCell>
                                <TableCell>Sender</TableCell>
                                <TableCell>Receiver</TableCell>
                                <TableCell>Flow Rate</TableCell>
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {flows!.map((flow: Flow, index) => (
                                <TableRow
                                    key={index}
                                    sx={{
                                        "&:last-child td, &:last-child th": {
                                            border: 0,
                                        },
                                    }}
                                >
                                    <TableCell>{networkName}</TableCell>
                                    <TableCell>{flow.superToken}</TableCell>
                                    <TableCell>{flow.sender}</TableCell>
                                    <TableCell>{flow.receiver}</TableCell>
                                    <TableCell>{flow.flowRate}</TableCell>
                                </TableRow>
                            ))}
                        </TableBody>
                    </Table>
                </TableContainer>
            )}
        </>
    );
};
