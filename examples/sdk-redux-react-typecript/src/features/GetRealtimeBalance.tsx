import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useState,
} from "react";
import { Loader } from "../Loader";
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
import { SignerContext } from "../SignerContext";
import { Error } from "../Error";
import { FlowingBalance } from "../FlowingBalance";
import { ethers } from "ethers";
import { sfApi } from "../redux/store";

export const GetRealtimeBalance: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);
    const [superTokenAddress, setSuperTokenAddress] = useState("");

    const [
        queryTrigger,
        { data: realtimeBalance, isLoading, error, isUninitialized },
        ,
    ] = sfApi.useLazyGetRealtimeBalanceQuery({
        pollingInterval: 5000, // Polling is not necessary (once is enough), just for visualization.
    });

    const triggerQuery = () => {
        queryTrigger({
            chainId: chainId,
            accountAddress: accountAddress,
            superTokenAddress: superTokenAddress,
            estimationTimestamp: undefined,
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
                        Get
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
                                        <TableCell>Timestamp</TableCell>
                                        <TableCell>Balance</TableCell>
                                        <TableCell>Flow Rate</TableCell>
                                        <TableCell>Deposit</TableCell>
                                        <TableCell>Owed Deposit</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                    <TableRow>
                                        <TableCell>
                                            {realtimeBalance!.timestamp}
                                        </TableCell>
                                        <TableCell>
                                            {
                                                realtimeBalance!
                                                    .availableBalanceWei
                                            }
                                            <br />
                                            <FlowingBalance
                                                balanceWei={
                                                    realtimeBalance!
                                                        .availableBalanceWei
                                                }
                                                flowRateWei={
                                                    realtimeBalance!
                                                        .netFlowRateWei
                                                }
                                                balanceTimestamp={
                                                    realtimeBalance!.timestamp
                                                }
                                            />
                                            <br />
                                            <FlowingBalance
                                                format={(x) =>
                                                    ethers.utils.formatUnits(x)
                                                }
                                                balanceWei={
                                                    realtimeBalance!
                                                        .availableBalanceWei
                                                }
                                                flowRateWei={
                                                    realtimeBalance!
                                                        .netFlowRateWei
                                                }
                                                balanceTimestamp={
                                                    realtimeBalance!.timestamp
                                                }
                                            />
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.netFlowRateWei}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.depositWei}
                                        </TableCell>
                                        <TableCell>
                                            {realtimeBalance!.owedDepositWei}
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
