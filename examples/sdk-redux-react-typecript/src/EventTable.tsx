import React, { FC, ReactElement, useContext, useState } from "react";
import {
    AllEvents,
    useFetchAllEventsQuery,
} from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import {
    Pagination,
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
} from "@mui/material";
import { SignerContext } from "./SignerContext";

const pageSize = 10;

export const EventTable: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const {
        data: pagedResult,
        isFetching,
        isLoading,
    } = useFetchAllEventsQuery({
        chainId: chainId,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    const events: AllEvents[] = pagedResult ? pagedResult.data : [];
    const hasNextPage: boolean = pagedResult ? pagedResult.hasNextPage : false;

    const handleChange = (event: React.ChangeEvent<unknown>, value: number) => {
        setPage(value);
    };

    return (
        <>
            {isFetching ? (
                <Loader />
            ) : (
                <TableContainer>
                    <Table aria-label="simple table">
                        <TableHead>
                            <TableRow>
                                <TableCell>Block</TableCell>
                                <TableCell>Event</TableCell>
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {events!.map((flow: AllEvents, index) => (
                                <TableRow
                                    key={index}
                                    sx={{
                                        "&:last-child td, &:last-child th": {
                                            border: 0,
                                        },
                                    }}
                                >
                                    <TableCell>{flow.blockNumber}</TableCell>
                                    <TableCell>{flow.__typename}</TableCell>
                                </TableRow>
                            ))}
                        </TableBody>
                    </Table>
                </TableContainer>
            )}
            {isLoading ? (
                <></>
            ) : (
                <Pagination
                    count={hasNextPage ? page + 1 : page}
                    size="small"
                    onChange={handleChange}
                />
            )}
        </>
    );
};
