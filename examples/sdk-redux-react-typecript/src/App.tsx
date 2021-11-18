import React, { FC, ReactElement, useState } from "react";
import { Box, Container, Typography } from "@mui/material";
import { InitializeSuperfluidSdk } from "./InitializeSuperfluidSdk";
import { Framework } from "@superfluid-finance/sdk-redux";
import { Loader } from "./Loader";
import { SignerContext } from "./SignerContext";
import { ListStreams } from "./ListStreams";
import { TransactionTable } from "./TransactionTable";
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
import { ListSuperTokens } from "./ListSuperTokens";
import { GetRealtimeBalance } from "./GetRealtimeBalance";
import { CreateStream } from "./CreateStream";
import { UpgradeToSuperToken } from "./UpgradeToSuperToken";
import { UpdateStream } from "./UpdateStream";
import { DeleteStream } from "./DeleteStream";
import { DowngradeFromSuperToken } from "./DowngradeFromSuperToken";
import { CreateIndex } from "./CreateIndex";
import { DistributeToIndex } from "./DistributeToIndex";
import { ApproveIndexSubscription } from "./ApproveIndexSubscription";
import { UpdateIndexSubscriptionUnits } from "./UpdateIndexSubscriptionUnits";
import {ClaimFromIndexSubscription} from "./ClaimFromIndexSubscription";
import {DeleteIndexSubscription} from "./DeleteIndexSubscription";
import {RevokeIndexSubscription} from "./RevokeIndexSubscription";

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
                            <SdkListItem title="Transactions">
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
        <Box>
            <ListItemButton onClick={() => setIsListEventsOpen(!isOpen)}>
                <ListItemIcon>
                    <InboxIcon />
                </ListItemIcon>
                <ListItemText primary={title} />
                {isOpen ? <ExpandLess /> : <ExpandMore />}
            </ListItemButton>
            <Collapse in={isOpen} timeout="auto" unmountOnExit>
                <Box maxWidth="xl">{children}</Box>
            </Collapse>
        </Box>
    );
};

export default App;
