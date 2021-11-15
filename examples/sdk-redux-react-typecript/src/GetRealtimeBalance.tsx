import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useState,
} from "react";
import { useLazyGetRealtimeBalanceQuery } from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import {
    Button,
    FormGroup,
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

export const GetRealtimeBalance: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);
    const [superTokenAddress, setSuperTokenAddress] = useState("");

    const [
        queryTrigger,
        {
            data: realtimeBalance,
            isLoading,
            error,
            isUninitialized,
        },
        ,
    ] = useLazyGetRealtimeBalanceQuery({
        pollingInterval: 5000,
    });

    const triggerQuery = () => {
        queryTrigger({
            chainId: chainId,
            accountAddress: accountAddress,
            superTokenAddress: superTokenAddress,
        });
    };

    return (
        <>
            <form
                onSubmit={(e: SyntheticEvent) => {
                    e.preventDefault();
                    triggerQuery();
                }}
            >
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
                    <Button variant="contained" type="submit">
                        Trigger
                    </Button>
                </FormGroup>
            </form>
            {!isUninitialized && (
                <>
                    {isLoading ? (
                        <Loader />
                    ) : error ? (
                        <Error
                            error={error}
                            retry={() => {
                                triggerQuery();
                            }}
                        />
                    ) : (
                        <TableContainer>
                            <Table aria-label="simple table">
                                <TableHead>
                                    <TableRow>
                                        <TableCell>Balance</TableCell>
                                        <TableCell>Deposit</TableCell>
                                        <TableCell>Owed Deposit</TableCell>
                                        <TableCell>Timestamp</TableCell>
                                        <TableCell>Flow Rate</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    <TableRow>
                                        <TableCell>
                                            {realtimeBalance!.availableBalance}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.deposit}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.owedDeposit}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.timestamp}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.netFlowRate}
                                        </TableCell>
                                    </TableRow>
                                </TableBody>
                            </Table>
                        </TableContainer>
                    )}
                </>
            )}
        </>
    );
};
