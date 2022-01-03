import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    ISuperToken,
} from "@superfluid-finance/sdk-core";
import { Loader } from "../Loader";
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
    TextField,
} from "@mui/material";
import { SignerContext } from "../SignerContext";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

const pageSize = 10;

export const ListSuperTokens: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);
    const [isListed, setIsListed] = useState<boolean | undefined>();

    useEffect(() => {
        setPage(1);
    }, [queryChainId, isListed]);

    const {
        data: pagedSuperTokens,
        isFetching,
        isLoading,
        error,
        refetch,
    } = sfApi.useListSuperTokensQuery({
        isListed: isListed,
        chainId: queryChainId,
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
                    <TextField
                        sx={{ m: 1 }}
                        label="Chain ID"
                        value={queryChainId}
                        onChange={(e) =>
                            setQueryChainId(Number(e.currentTarget.value))
                        }
                    />
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
                                        <TableCell>
                                            createdAtTimestamp
                                        </TableCell>
                                        <TableCell>
                                            createdAtBlockNumber
                                        </TableCell>
                                        <TableCell>name</TableCell>
                                        <TableCell>symbol</TableCell>
                                        <TableCell>isListed</TableCell>
                                        <TableCell>underlyingAddress</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedSuperTokens!.data.map(
                                        (superToken: ISuperToken) => (
                                            <TableRow key={superToken.id}>
                                                <TableCell>
                                                    {
                                                        superToken.createdAtTimestamp
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        superToken.createdAtBlockNumber
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {superToken.name}
                                                </TableCell>
                                                <TableCell>
                                                    {superToken.symbol}
                                                </TableCell>
                                                <TableCell>
                                                    {superToken.isListed}
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        superToken.underlyingAddress
                                                    }
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
                                pagedSuperTokens.nextPaging ? page + 1 : page
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
