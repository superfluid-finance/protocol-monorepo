import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useState,
} from "react";
import {
    Alert,
    Box,
    Button,
    Container,
    FormGroup,
    TextField,
    Typography,
} from "@mui/material";
import { Framework } from "@superfluid-finance/js-sdk";
import { InitializeSuperfluidSdk } from "./InitializeSuperfluidSdk";
import {
    useCreateFlowMutation,
    useCreateOrUpdateOrDeleteFlowMutation,
    useDeleteFlowMutation,
    useUpdateFlowMutation,
} from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import { SignerContext } from "./SignerContext";
import { StreamTable } from "./StreamTable";
import { TransactionTable } from "./TransactionTable";
import { SerializedError } from "@reduxjs/toolkit";
import { ChainId } from "@superfluid-finance/sdk-core";

export const CreateStream: FC = (): ReactElement => {
    const [
        createFlow,
        { isLoading: createFlowIsLoading, error: createFlowError },
    ] = useCreateFlowMutation();

    const [
        updateFlow,
        { isLoading: updateFlowIsLoading, error: updateFlowError },
    ] = useUpdateFlowMutation();

    const [
        deleteFlow,
        { isLoading: deleteFlowIsLoading, error: deleteFlowError },
    ] = useDeleteFlowMutation();

    const [
        createOrUpdateOrDeleteFlow,
        {
            isLoading: createOrUpdateOrDeleteIsLoading,
            error: createOrUpdateOrDeleteError,
        },
    ] = useCreateOrUpdateOrDeleteFlowMutation();

    const [chainId, signerAddress] = useContext(SignerContext);

    const [receiver, setReceiver] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [flowRate, setFlowRate] = useState<string>("");

    const isAnythingLoading =
        createFlowIsLoading ||
        updateFlowIsLoading ||
        deleteFlowIsLoading ||
        createOrUpdateOrDeleteIsLoading;

    const errors = [
        createFlowError,
        updateFlowError,
        deleteFlowError,
        createOrUpdateOrDeleteError,
    ].filter((item): item is Error | SerializedError => !!item);

    const handleCreateStream = (e: SyntheticEvent) => {
        createFlow({
            sender: signerAddress,
            receiver,
            flowRate,
            chainId,
            superToken,
        });
    };

    const handleUpdateStream = (e: SyntheticEvent) => {
        updateFlow({
            sender: signerAddress,
            receiver,
            flowRate,
            chainId,
            superToken,
        });
    };

    const handleDeleteStream = (e: SyntheticEvent) => {
        deleteFlow({
            sender: signerAddress,
            receiver,
            chainId,
            superToken,
        });
    };

    const handleCreateOrUpdateOrDeleteStream = (e: SyntheticEvent) => {
        createOrUpdateOrDeleteFlow({
            sender: signerAddress,
            receiver,
            flowRate,
            chainId,
            superToken,
        });
    };

    return (
        <>
            {isAnythingLoading ? (
                <Loader />
            ) : (
                <>
                    {errors.length ? (
                        errors.map((error) => (
                            <Alert sx={{ m: 1 }} severity="error">
                                {error.message}
                            </Alert>
                        ))
                    ) : (
                        <></>
                    )}
                    <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                        <FormGroup>
                            <TextField
                                sx={{ m: 1 }}
                                label="Receiver"
                                onChange={(e) =>
                                    setReceiver(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="SuperToken"
                                onChange={(e) =>
                                    setSuperToken(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="Flow Rate"
                                type="number"
                                onChange={(e) =>
                                    setFlowRate(e.currentTarget.value)
                                }
                            />
                            <Button
                                sx={{ m: 1 }}
                                type="submit"
                                variant="contained"
                                fullWidth={true}
                                onClick={handleCreateStream}
                            >
                                Create
                            </Button>
                            <Button
                                sx={{ m: 1 }}
                                type="submit"
                                variant="contained"
                                fullWidth={true}
                                onClick={handleUpdateStream}
                            >
                                Update
                            </Button>
                            <Button
                                sx={{ m: 1 }}
                                type="submit"
                                variant="contained"
                                fullWidth={true}
                                onClick={handleDeleteStream}
                            >
                                Delete
                            </Button>
                            <Button
                                sx={{ m: 1 }}
                                type="submit"
                                variant="contained"
                                fullWidth={true}
                                onClick={handleCreateOrUpdateOrDeleteStream}
                            >
                                Create | Update | Delete
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};

function App() {
    const [superfluidSdk, setSuperfluidSdk] = useState<Framework | undefined>();

    const [signerAddress, setSignerAddress] = useState<string | undefined>();
    const [chainId, setChainId] = useState<ChainId | undefined>();

    const onSuperfluidSdkInitialized = async (superfluidSdk: Framework) => {
        setSuperfluidSdk(superfluidSdk);

        superfluidSdk.ethers
            .getSigner()
            .getAddress()
            .then((address) => setSignerAddress(address));

        superfluidSdk.ethers
            .getNetwork()
            .then((network) => setChainId(network.chainId as ChainId));
    };

    return (
        <Container maxWidth={false}>
            <Box sx={{ my: 4 }}>
                <Typography variant="h2" component="h2" gutterBottom>
                    Redux-SDK example
                </Typography>
                {!superfluidSdk ? (
                    <InitializeSuperfluidSdk
                        onSuperfluidSdkInitialized={(x) =>
                            onSuperfluidSdkInitialized(x)
                        }
                    />
                ) : !chainId || !signerAddress ? (
                    <Loader />
                ) : (
                    <SignerContext.Provider
                        value={[chainId, signerAddress]}
                    >
                        <Box maxWidth="sm">
                            <Typography sx={{ mb: 4 }}>
                                You are connected. You are on network [
                                {chainId}] and your wallet address is [
                                {signerAddress}].
                            </Typography>
                            <Typography
                                variant="h3"
                                component="h3"
                                gutterBottom
                            >
                                Create Stream
                            </Typography>
                            <CreateStream />
                        </Box>
                        <Box maxWidth="xl">
                            <Typography
                                variant="h3"
                                component="h3"
                                gutterBottom
                            >
                                Active Streams
                            </Typography>
                            <StreamTable />
                        </Box>
                        <Box maxWidth="xl">
                            <Typography
                                variant="h3"
                                component="h3"
                                gutterBottom
                            >
                                Transactions
                            </Typography>
                            <TransactionTable />
                        </Box>
                    </SignerContext.Provider>
                )}
            </Box>
        </Container>
    );
}

export default App;
