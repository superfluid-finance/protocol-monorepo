import { FC, PropsWithChildren, ReactElement, useState } from "react";
import { Box, Container, Paper, Typography } from "@mui/material";
import { InitializeSuperfluidSdk } from "./InitializeSuperfluidSdk";
import { Framework } from "@superfluid-finance/sdk-core";
import { Loader } from "./Loader";
import { SignerContext } from "./SignerContext";
import { TransactionTable } from "./features/TransactionTable";
import ListSubheader from "@mui/material/ListSubheader";
import List from "@mui/material/List";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import Collapse from "@mui/material/Collapse";
import ExpandLess from "@mui/icons-material/ExpandLess";
import ExpandMore from "@mui/icons-material/ExpandMore";
import { FlowCreate } from "./features/FlowCreate";
import { SuperTokenUpgrade } from "./features/SuperTokenUpgrade";
import { FlowUpdate } from "./features/FlowUpdate";
import { DeleteStream } from "./features/DeleteStream";
import { SuperTokenDowngrade } from "./features/SuperTokenDowngrade";
import { IndexCreate } from "./features/IndexCreate";
import { DistributeToIndex } from "./features/DistributeToIndex";
import { IndexSubscriptionApprove } from "./features/IndexSubscriptionApprove";
import { IndexUpdateSubscriptionUnits } from "./features/IndexUpdateSubscriptionUnits";
import { IndexSubscriptionClaim } from "./features/IndexSubscriptionClaim";
import { IndexDeleteSubscription } from "./features/IndexDeleteSubscription";
import { IndexSubscriptionRevoke } from "./features/IndexSubscriptionRevoke";
import { MonitorForEventsToInvalidateCache } from "./features/MonitorForEventsToInvalidateCache";
import { Account } from "./features/generic-entity-queries/Account";
import { Accounts } from "./features/generic-entity-queries/Accounts";
import { AccountTokenSnapshots } from "./features/generic-entity-queries/AccountTokenSnapshots";
import { Indexes } from "./features/generic-entity-queries/Indexes";
import { Streams } from "./features/generic-entity-queries/Stream";
import { StreamPeriods } from "./features/generic-entity-queries/StreamPeriods";
import { Tokens } from "./features/generic-entity-queries/Tokens";
import { IndexSubscriptions } from "./features/generic-entity-queries/IndexSubscriptions";
import { TokenStatistics } from "./features/generic-entity-queries/TokenStatistics";
import { ethers, Signer } from "ethers";
import { chains } from "./wagmiAndRainbowKit";

function App() {
    const [superfluidSdk, setSuperfluidSdk] = useState<Framework | undefined>();
    const [signerAddress, setSignerAddress] = useState<string | undefined>();
    const [signer, setSigner] = useState<Signer>();
    const [chainId, setChainId] = useState<number | undefined>();

    const onSuperfluidSdkInitialized = async (
        superfluidSdk: Framework,
        provider: ethers.providers.Provider,
        signer: Signer
    ) => {
        setSuperfluidSdk(superfluidSdk);
        setSigner(signer);

        signer.getAddress().then((address) => setSignerAddress(address));
        provider.getNetwork().then((network) => setChainId(network.chainId));
    };

    return (
        <Container maxWidth={false}>
            <Box sx={{ my: 4 }}>
                <Typography variant="h2" component="h2" gutterBottom>
                    SDK-Redux demo
                </Typography>
                <Typography variant="caption">
                    <p>
                        SDK-Redux is a batteries-included* application framework
                        for building front-end dApps that interact with the
                        Superfluid Protocol. Its foremost goal is to make
                        building dashboard and explorer like applications a
                        breeze.
                    </p>
                    <p>
                        *{" "}
                        <em>
                            Some batteries still missing because of early active
                            development :)
                        </em>
                    </p>
                    <p>Main features:</p>
                    <ul>
                        <li>
                            Reacts Hooks for interacting with the Superfluid
                            SDK-Core (the underlying stateless SDK)
                        </li>
                        <li>
                            Queried data is automatically cached and avoids
                            duplicate requests for the same data
                        </li>
                        <li>
                            Tracking loading and transaction states for UI
                            spinners and notifications
                        </li>
                        <li>
                            Monitoring blockchain events and transactions for
                            cache invalidation (reorg included) to re-fetch data
                        </li>
                        <li>Cross-chain querying of data</li>
                    </ul>
                    <p>Available chains (name, chainId):</p>
                    <ul>
                        {chains.map((chain) => (
                            <li key={chain.id}>
                                {chain.name}: {chain.id}
                            </li>
                        ))}
                    </ul>
                </Typography>
                <InitializeSuperfluidSdk
                    onSuperfluidSdkInitialized={(sdk, provider, signer) =>
                        onSuperfluidSdkInitialized(sdk, provider, signer)
                    }
                />
                {!!superfluidSdk && (!chainId || !signerAddress || !signer) && (
                    <Loader />
                )}
                {!!chainId && !!signerAddress && !!signer && (
                    <SignerContext.Provider
                        value={[chainId, signerAddress, signer]}
                    >
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
                                    SDK-Redux Query Features
                                </ListSubheader>
                            }
                        >
                            <SdkListItem title="Monitor For Events To Invalidate Cache">
                                <MonitorForEventsToInvalidateCache />
                            </SdkListItem>
                        </List>
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
                                    SDK-Redux Transaction Features
                                </ListSubheader>
                            }
                        >
                            <SdkListItem title="Transaction tracking">
                                <TransactionTable />
                            </SdkListItem>
                            <SdkListItem title="Upgrade To SuperToken">
                                <SuperTokenUpgrade />
                            </SdkListItem>
                            <SdkListItem title="Downgrade From SuperToken">
                                <SuperTokenDowngrade />
                            </SdkListItem>
                            <SdkListItem title="Create Index">
                                <IndexCreate />
                            </SdkListItem>
                            <SdkListItem title="Update Index Subscription Units">
                                <IndexUpdateSubscriptionUnits />
                            </SdkListItem>
                            <SdkListItem title="Distribute To Index">
                                <DistributeToIndex />
                            </SdkListItem>
                            <SdkListItem title="Approve Index Subscription">
                                <IndexSubscriptionApprove />
                            </SdkListItem>
                            <SdkListItem title="Claim From Index Subscription">
                                <IndexSubscriptionClaim />
                            </SdkListItem>
                            <SdkListItem title="Delete Index Subscription">
                                <IndexDeleteSubscription />
                            </SdkListItem>
                            <SdkListItem title="Revoke Index Subscription">
                                <IndexSubscriptionRevoke />
                            </SdkListItem>
                            <SdkListItem title="Create Stream">
                                <FlowCreate />
                            </SdkListItem>
                            <SdkListItem title="Update Stream">
                                <FlowUpdate />
                            </SdkListItem>
                            <SdkListItem title="Delete Stream">
                                <DeleteStream />
                            </SdkListItem>
                        </List>
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
                                    SDK-Redux Entity Queries
                                </ListSubheader>
                            }
                        >
                            <SdkListItem title="Account">
                                <Account />
                            </SdkListItem>
                            <SdkListItem title="Accounts">
                                <Accounts />
                            </SdkListItem>
                            <SdkListItem title="AccountTokenSnapshots">
                                <AccountTokenSnapshots />
                            </SdkListItem>
                            <SdkListItem title="Indexes">
                                <Indexes />
                            </SdkListItem>
                            <SdkListItem title="IndexSubscriptions">
                                <IndexSubscriptions />
                            </SdkListItem>
                            <SdkListItem title="Streams">
                                <Streams />
                            </SdkListItem>
                            <SdkListItem title="StreamPeriods">
                                <StreamPeriods />
                            </SdkListItem>
                            <SdkListItem title="Tokens">
                                <Tokens />
                            </SdkListItem>
                            <SdkListItem title="TokenStatistics">
                                <TokenStatistics />
                            </SdkListItem>
                        </List>
                    </SignerContext.Provider>
                )}
            </Box>
        </Container>
    );
}

export const SdkListItem: FC<PropsWithChildren<{ title: string }>> = ({
    children,
    title,
}): ReactElement => {
    const [isOpen, setIsListEventsOpen] = useState(false);

    return (
        <Box mb={1}>
            <Paper variant="outlined">
                <ListItemButton onClick={() => setIsListEventsOpen(!isOpen)}>
                    <ListItemIcon>
                        {isOpen ? <ExpandLess /> : <ExpandMore />}
                    </ListItemIcon>
                    <ListItemText primary={title} />
                </ListItemButton>
                <Collapse in={isOpen} timeout="auto" unmountOnExit>
                    <Box p={2}>{children}</Box>
                </Collapse>
            </Paper>
        </Box>
    );
};

export default App;
