import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    useListUserInteractedSuperTokensQuery,
    ILightAccountTokenSnapshot,
} from "@superfluid-finance/sdk-redux";
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

export const ListUserInteractedSuperTokens: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);

    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);
    const [superTokenAddress, setSuperTokenAddress] = useState("");

    useEffect(() => {
        setPage(1);
    }, [chainId, superTokenAddress, accountAddress]);

    const {
        data: pagedIndexSubscriptions,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListUserInteractedSuperTokensQuery({
        accountAddress,
        superTokenAddress,
        chainId: chainId,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
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
                                        <TableCell>Index</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedIndexSubscriptions!.data.map(
                                        (
                                            tokenSnapshot: ILightAccountTokenSnapshot
                                        ) => (
                                            <TableRow key={tokenSnapshot.id}>
                                                <TableCell>
                                                    <pre>
                                                        {JSON.stringify(
                                                            tokenSnapshot
                                                        )}
                                                    </pre>
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
                                pagedIndexSubscriptions.hasNextPage
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
