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
import { InitializeSuperfluidSdk } from "./InitializeSuperfluidSdk";
import {
    Framework,
    useCreateFlowMutation,
    useDeleteFlowMutation,
    useUpdateFlowMutation,
} from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import { SignerContext } from "./SignerContext";
import { StreamTable } from "./StreamTable";
import { TransactionTable } from "./TransactionTable";
import { SerializedError } from "@reduxjs/toolkit";
import { Web3Provider } from "@ethersproject/providers";
import { ListEvents } from "./ListEvents";
import { ListIndexes } from "./ListIndexes";
import ListSubheader from "@mui/material/ListSubheader";
import List from "@mui/material/List";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import Collapse from "@mui/material/Collapse";
import InboxIcon from "@mui/icons-material/MoveToInbox";
import ExpandLess from "@mui/icons-material/ExpandLess";
import ExpandMore from "@mui/icons-material/ExpandMore";
import { ListIndexSubscriptions } from "./ListIndexSubscriptions";
import { ListUserInteractedSuperTokens } from "./ListUserInteractedSuperTokens";
import {ListSuperTokens} from "./ListSuperTokens";

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

    const [chainId, signerAddress] = useContext(SignerContext);

    const [receiver, setReceiver] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [flowRate, setFlowRate] = useState<string>("");

    const isAnythingLoading =
        createFlowIsLoading || updateFlowIsLoading || deleteFlowIsLoading;

    const errors = [createFlowError, updateFlowError, deleteFlowError].filter(
        (item): item is Error | SerializedError => !!item
    );

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
    const [chainId, setChainId] = useState<number | undefined>();

    const onSuperfluidSdkInitialized = async (
        superfluidSdk: Framework,
        provider: Web3Provider
    ) => {
        setSuperfluidSdk(superfluidSdk);

        provider
            .getSigner()
            .getAddress()
            .then((address) => setSignerAddress(address));

        provider.getNetwork().then((network) => setChainId(network.chainId));
    };

    const [isListIndexesOpen, setIsListIndexesOpen] = useState(false);
    const [isListIndexSubscriptionsOpen, setIsListIndexSubscriptionsOpen] =
        useState(false);
    const [isActiveStreamsOpen, setIsActiveStreamsOpen] = useState(false);
    const [isCreateStreamOpen, setIsCreateStreamOpen] = useState(false);
    const [isTransactionsOpen, setIsTransactionsOpen] = useState(false);
    const [isListEventsOpen, setIsListEventsOpen] = useState(false);
    const [isListSuperTokensOpen, setIsListSuperTokensOpen] = useState(false);
    const [
        isListUserInteractedSuperTokensOpen,
        setIsListUserInteractedSuperTokensOpen,
    ] = useState(false);

    return (
        <Container maxWidth={false}>
            <Box sx={{ my: 4 }}>
                <Typography variant="h2" component="h2" gutterBottom>
                    SDK-Redux example
                </Typography>
                {!superfluidSdk ? (
                    <InitializeSuperfluidSdk
                        onSuperfluidSdkInitialized={(x, provider) =>
                            onSuperfluidSdkInitialized(x, provider)
                        }
                    />
                ) : !chainId || !signerAddress ? (
                    <Loader />
                ) : (
                    <SignerContext.Provider value={[chainId, signerAddress]}>
                        <Box maxWidth="sm">
                            <Typography sx={{ mb: 4 }}>
                                You are connected. You are on network [{chainId}
                                ] and your wallet address is [{signerAddress}].
                            </Typography>
                        </Box>

                        <List
                            sx={{
                                width: "100%",
                                bgcolor: "background.paper",
                            }}
                            component="nav"
                            aria-labelledby="nested-list-subheader"
                            subheader={
                                <ListSubheader
                                    component="div"
                                    id="nested-list-subheader"
                                >
                                    SDK-Redux functionalities
                                </ListSubheader>
                            }
                        >
                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsTransactionsOpen(
                                            !isTransactionsOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="Transactions" />
                                    {isTransactionsOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isTransactionsOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <TransactionTable />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsListSuperTokensOpen(
                                            !isListSuperTokensOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="List SuperTokens" />
                                    {isListSuperTokensOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isListSuperTokensOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <ListSuperTokens />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsListUserInteractedSuperTokensOpen(
                                            !isListUserInteractedSuperTokensOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="List User-Interacted SuperTokens" />
                                    {isListUserInteractedSuperTokensOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isListUserInteractedSuperTokensOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <ListUserInteractedSuperTokens />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsCreateStreamOpen(
                                            !isCreateStreamOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="Create Stream" />
                                    {isCreateStreamOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isCreateStreamOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <CreateStream />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsActiveStreamsOpen(
                                            !isActiveStreamsOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="Active Streams" />
                                    {isActiveStreamsOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isActiveStreamsOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <StreamTable />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsListIndexesOpen(!isListIndexesOpen)
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="List Indexes" />
                                    {isListIndexesOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isListIndexesOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <ListIndexes />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsListIndexSubscriptionsOpen(
                                            !isListIndexSubscriptionsOpen
                                        )
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="List Index Subscriptions" />
                                    {isListIndexSubscriptionsOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isListIndexSubscriptionsOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <ListIndexSubscriptions />
                                    </Box>
                                </Collapse>
                            </Box>

                            <Box>
                                <ListItemButton
                                    onClick={() =>
                                        setIsListEventsOpen(!isListEventsOpen)
                                    }
                                >
                                    <ListItemIcon>
                                        <InboxIcon />
                                    </ListItemIcon>
                                    <ListItemText primary="List Events" />
                                    {isListEventsOpen ? (
                                        <ExpandLess />
                                    ) : (
                                        <ExpandMore />
                                    )}
                                </ListItemButton>
                                <Collapse
                                    in={isListEventsOpen}
                                    timeout="auto"
                                    unmountOnExit
                                >
                                    <Box maxWidth="xl">
                                        <ListEvents />
                                    </Box>
                                </Collapse>
                            </Box>
                        </List>
                    </SignerContext.Provider>
                )}
            </Box>
        </Container>
    );
}

export default App;
