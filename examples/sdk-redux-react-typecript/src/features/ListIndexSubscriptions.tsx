import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { Loader } from "../Loader";
import {
    FormControl,
    FormControlLabel,
    FormGroup,
    Pagination,
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
    TextField,
    RadioGroup,
    Radio,
} from "@mui/material";
import { SignerContext } from "../SignerContext";
import { Error } from "../Error";
import { sfApi } from "../redux/store";
import { IIndexSubscription } from "@superfluid-finance/sdk-core";

const pageSize = 10;

export const ListIndexSubscriptions: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);

    const [subscriberAddress, setSubscriberAddress] =
        useState<string>(signerAddress);
    const [isApproved, setIsApproved] = useState<boolean | undefined>();

    useEffect(() => {
        setPage(1);
    }, [queryChainId, isApproved, subscriberAddress]);

    const {
        data: pagedIndexSubscriptions,
        isFetching,
        isLoading,
        error,
        refetch,
    } = sfApi.useListIndexSubscriptionsQuery({
        chainId: queryChainId,
        subscriberAddress,
        approved: isApproved,
        skip: (page - 1) * pageSize,
        take: pageSize,
    });

    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setIsApproved((event.target as HTMLInputElement).value === "true");
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
                    <TextField
                        sx={{ m: 1 }}
                        label="Subscriber Address"
                        value={subscriberAddress}
                        onChange={(e) =>
                            setSubscriberAddress(e.currentTarget.value)
                        }
                    />
                    <FormControl component="fieldset">
                        <RadioGroup
                            row
                            name="row-radio-buttons-group"
                            value={isApproved}
                            onChange={handleChange}
                        >
                            <FormControlLabel
                                value="true"
                                control={<Radio />}
                                label="Approved"
                            />
                            <FormControlLabel
                                value="false"
                                control={<Radio />}
                                label="Not Approved"
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
                                        <TableCell>
                                            updatedAtTimestamp
                                        </TableCell>
                                        <TableCell>
                                            updatedAtBlockNumber
                                        </TableCell>
                                        <TableCell>subscriber</TableCell>
                                        <TableCell>approved</TableCell>
                                        <TableCell>units</TableCell>
                                        <TableCell>
                                            totalAmountReceivedUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>
                                            indexValueUntilUpdatedAt
                                        </TableCell>
                                        <TableCell>index</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    {pagedIndexSubscriptions!.data.map(
                                        (
                                            indexSubscription: IIndexSubscription
                                        ) => (
                                            <TableRow
                                                key={indexSubscription.id}
                                            >
                                                <TableCell>
                                                    {
                                                        indexSubscription.createdAtTimestamp
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        indexSubscription.createdAtBlockNumber
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        indexSubscription.updatedAtTimestamp
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        indexSubscription.updatedAtBlockNumber
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        indexSubscription.subscriber
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {indexSubscription.approved}
                                                </TableCell>
                                                <TableCell>
                                                    {indexSubscription.units}
                                                </TableCell>
                                                <TableCell>
                                                    {
                                                        indexSubscription.totalAmountReceivedUntilUpdatedAt
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    W
                                                    {
                                                        indexSubscription.indexValueUntilUpdatedAt
                                                    }
                                                </TableCell>
                                                <TableCell>
                                                    {indexSubscription.index.id}
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
                                pagedIndexSubscriptions.nextPaging
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
