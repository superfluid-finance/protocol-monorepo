import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    useListIndexSubscriptionsQuery,
    IIndexSubscription,
} from "@superfluid-finance/sdk-redux";
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

const pageSize = 10;

export const ListIndexSubscriptions: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);

    const [subscriberAddress, setSubscriberAddress] =
        useState<string>(signerAddress);
    const [isApproved, setIsApproved] = useState<boolean | undefined>();

    useEffect(() => {
        setPage(1);
    }, [chainId, isApproved, subscriberAddress]);

    const {
        data: pagedIndexSubscriptions,
        isFetching,
        isLoading,
        error,
        refetch,
    } = useListIndexSubscriptionsQuery({
        chainId: chainId,
        subscriber: subscriberAddress,
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
                                        <TableCell>Index</TableCell>
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
                                                    <pre>
                                                        {JSON.stringify(
                                                            indexSubscription
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
