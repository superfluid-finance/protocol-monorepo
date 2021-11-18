import React, { FC, ReactElement, useContext } from "react";
import {
    TransactionTracking,
    superfluidTransactionSelectors
} from "@superfluid-finance/sdk-redux";
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
import { useAppSelector } from "./redux/hooks";

export const TransactionTable: FC = (): ReactElement => {
    const [networkName, signerAddress] = useContext(SignerContext);

    const transactions = useAppSelector((state) =>
        superfluidTransactionSelectors.selectAll(state.transactions)
    );

    return (
        <>
            {
                <TableContainer>
                    <Table aria-label="simple table">
                        <TableHead>
                            <TableRow>
                                <TableCell>Network</TableCell>
                                <TableCell>Hash</TableCell>
                                <TableCell>Status</TableCell>
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {transactions!.map(
                                (transaction: TransactionTracking) => (
                                    <TableRow
                                        key={transaction.hash}
                                        sx={{
                                            "&:last-child td, &:last-child th":
                                                {
                                                    border: 0,
                                                },
                                        }}
                                    >
                                        <TableCell>{networkName}</TableCell>
                                        <TableCell>
                                            {transaction.hash}
                                        </TableCell>
                                        <TableCell>
                                            {transaction.status.toString()}
                                        </TableCell>
                                    </TableRow>
                                )
                            )}
                        </TableBody>
                    </Table>
                </TableContainer>
            }
        </>
    );
};
