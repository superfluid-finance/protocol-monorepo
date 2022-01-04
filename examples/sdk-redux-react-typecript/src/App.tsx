import React, { FC, ReactElement, useState } from "react";
import { Box, Container, Paper, Typography } from "@mui/material";
import { InitializeSuperfluidSdk } from "./InitializeSuperfluidSdk";
import { Framework } from "@superfluid-finance/sdk-core";
import { Loader } from "./Loader";
import { SignerContext } from "./SignerContext";
import { ListStreams } from "./features/ListStreams";
import { TransactionTable } from "./features/TransactionTable";
import { Web3Provider } from "@ethersproject/providers";
import { ListEvents } from "./features/ListEvents";
import { ListIndexes } from "./features/ListIndexes";
import ListSubheader from "@mui/material/ListSubheader";
import List from "@mui/material/List";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import Collapse from "@mui/material/Collapse";
import ExpandLess from "@mui/icons-material/ExpandLess";
import ExpandMore from "@mui/icons-material/ExpandMore";
import { ListIndexSubscriptions } from "./features/ListIndexSubscriptions";
import { ListUserInteractedSuperTokens } from "./features/ListUserInteractedSuperTokens";
import { ListSuperTokens } from "./features/ListSuperTokens";
import { GetRealtimeBalance } from "./features/GetRealtimeBalance";
import { CreateStream } from "./features/CreateStream";
import { UpgradeToSuperToken } from "./features/UpgradeToSuperToken";
import { UpdateStream } from "./features/UpdateStream";
import { DeleteStream } from "./features/DeleteStream";
import { DowngradeFromSuperToken } from "./features/DowngradeFromSuperToken";
import { CreateIndex } from "./features/CreateIndex";
import { DistributeToIndex } from "./features/DistributeToIndex";
import { ApproveIndexSubscription } from "./features/ApproveIndexSubscription";
import { UpdateIndexSubscriptionUnits } from "./features/UpdateIndexSubscriptionUnits";
import { ClaimFromIndexSubscription } from "./features/ClaimFromIndexSubscription";
import { DeleteIndexSubscription } from "./features/DeleteIndexSubscription";
import { RevokeIndexSubscription } from "./features/RevokeIndexSubscription";
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
                        <li>Ropsten: 3</li>
                        <li>Rinkeby: 4</li>
                        <li>Goerli: 5</li>
                        <li>Kovan: 42</li>
                        <li>xDai: 100</li>
                        <li>Matic: 137</li>
                        <li>Mumbai: 80001</li>
                    </ul>
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
                                    SDK-Redux Query Features
                                </ListSubheader>
                            }
                        >
                            <SdkListItem title="Real-Time Balance">
                                <GetRealtimeBalance />
                            </SdkListItem>
                            <SdkListItem title="List Streams">
                                <ListStreams />
                            </SdkListItem>
                            <SdkListItem title="List SuperTokens">
                                <ListSuperTokens />
                            </SdkListItem>
                            <SdkListItem title="List User-Interacted SuperTokens">
                                <ListUserInteractedSuperTokens />
                            </SdkListItem>
                            <SdkListItem title="List Indexes">
                                <ListIndexes />
                            </SdkListItem>
                            <SdkListItem title="List Index Subscriptions">
                                <ListIndexSubscriptions />
                            </SdkListItem>
                            <SdkListItem title="List Events">
                                <ListEvents />
                            </SdkListItem>
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
                                <UpgradeToSuperToken />
                            </SdkListItem>
                            <SdkListItem title="Downgrade From SuperToken">
                                <DowngradeFromSuperToken />
                            </SdkListItem>
                            <SdkListItem title="Create Index">
                                <CreateIndex />
                            </SdkListItem>
                            <SdkListItem title="Update Index Subscription Units">
                                <UpdateIndexSubscriptionUnits />
                            </SdkListItem>
                            <SdkListItem title="Distribute To Index">
                                <DistributeToIndex />
                            </SdkListItem>
                            <SdkListItem title="Approve Index Subscription">
                                <ApproveIndexSubscription />
                            </SdkListItem>
                            <SdkListItem title="Claim From Index Subscription">
                                <ClaimFromIndexSubscription />
                            </SdkListItem>
                            <SdkListItem title="Delete Index Subscription">
                                <DeleteIndexSubscription />
                            </SdkListItem>
                            <SdkListItem title="Revoke Index Subscription">
                                <RevokeIndexSubscription />
                            </SdkListItem>
                            <SdkListItem title="Create Stream">
                                <CreateStream />
                            </SdkListItem>
                            <SdkListItem title="Update Stream">
                                <UpdateStream />
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

export const SdkListItem: FC<{ title: string }> = ({
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
