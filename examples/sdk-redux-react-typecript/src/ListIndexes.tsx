import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { useListIndexesQuery, IIndex } from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
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
import { SignerContext } from "./SignerContext";
import { Error } from "./Error";

const pageSize = 10;

export const ListIndexes: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);

    const [indexId, setIndexId] = useState<string>("");
    const [publisherAddress, setPublisherAddress] = useState<string>("");
    const [superTokenAddress, setSuperTokenAddress] = useState<string>("");

    useEffect(() => {
        setPage(1);
    }, [chainId, indexId, publisherAddress, superTokenAddress]);

    const {
        data: pagedIndexes,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListIndexesQuery({
        chainId: chainId,
        indexId: indexId,
        publisher: publisherAddress,
        token: superTokenAddress,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <TextField
                        sx={{ m: 1 }}
                        label="Index ID"
                        onChange={(e) => setIndexId(e.currentTarget.value)}
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="Publisher Address"
                        onChange={(e) =>
                            setPublisherAddress(e.currentTarget.value)
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="SuperToken Address"
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
                                        <TableCell>Index</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedIndexes!.data.map((index: IIndex) => (
                                        <TableRow key={index.id}>
                                            <TableCell>
                                                <pre>
                                                    {JSON.stringify(index)}
                                                </pre>
                                            </TableCell>
                                        </TableRow>
                                    ))}
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                    {(pagedIndexes && !error) && (
                        <Pagination
                            count={pagedIndexes.hasNextPage ? page + 1 : page}
                            size="small"
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
