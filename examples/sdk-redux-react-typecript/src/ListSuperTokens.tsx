import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    useListSuperTokensQuery,
    ISuperToken,
} from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import {
    FormControl,
    FormControlLabel,
    FormGroup,
    Pagination,
    Radio,
    RadioGroup,
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
} from "@mui/material";
import { SignerContext } from "./SignerContext";
import { Error } from "./Error";

const pageSize = 10;

export const ListSuperTokens: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [isListed, setIsListed] = useState<boolean | undefined>();

    useEffect(() => {
        setPage(1);
    }, [chainId, isListed]);

    const {
        data: pagedSuperTokens,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListSuperTokensQuery({
        isListed: isListed,
        chainId: chainId,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setIsListed((event.target as HTMLInputElement).value === "true");
    };

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <FormControl component="fieldset">
                        <RadioGroup
                            row
                            name="row-radio-buttons-group"
                            value={isListed}
                            onChange={handleChange}
                        >
                            <FormControlLabel
                                value="true"
                                control={<Radio />}
                                label="Listed"
                            />
                            <FormControlLabel
                                value="false"
                                control={<Radio />}
                                label="Not Listed"
                            />
                        </RadioGroup>
                    </FormControl>
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
                                    {pagedSuperTokens!.data.map(
                                        (
                                            superToken: ISuperToken
                                        ) => (
                                            <TableRow key={superToken.id}>
                                                <TableCell>
                                                    <pre>
                                                        {JSON.stringify(
                                                            superToken
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
                    {pagedSuperTokens && !error && (
                        <Pagination
                            count={
                                pagedSuperTokens.hasNextPage
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
