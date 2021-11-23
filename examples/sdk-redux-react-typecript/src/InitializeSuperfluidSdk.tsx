import React, { FC, ReactElement } from "react";
import { Framework } from "@superfluid-finance/sdk-redux";
import { Web3Provider } from "@ethersproject/providers";
import { Button } from "@mui/material";
import Web3Modal from "web3modal";
import { superfluidFrameworkSource } from "./redux/store";

interface Props {
    onSuperfluidSdkInitialized: (sf: Framework, provider: Web3Provider) => void;
}

export const InitializeSuperfluidSdk: FC<Props> = ({
    onSuperfluidSdkInitialized,
}): ReactElement => {
    const handleWallet = async () => {
        const providerOptions = {
            /* See Provider Options Section */
        };

        const networkName = "goerli";
        const web3Modal = new Web3Modal({
            network: networkName, // optional
            cacheProvider: true, // optional
            providerOptions, // required
        });

        const web3ModalProvider = await web3Modal.connect();
        const ethersWeb3Provider = new Web3Provider(web3ModalProvider);

        const chainId = 5;
        const superfluidSdk = await Framework.create({
            chainId: chainId,
            provider: ethersWeb3Provider,
            customSubgraphQueriesEndpoint: "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-feature-goerli"
        });

        // @ts-ignore
        window.sf = superfluidSdk;

        superfluidFrameworkSource.setFramework(
            chainId,
            () => Promise.resolve(superfluidSdk)
        );
        superfluidFrameworkSource.setSigner(
            chainId,
            () => Promise.resolve(ethersWeb3Provider.getSigner())
        );

        onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);
    };

    return (
        <Button variant="contained" onClick={handleWallet}>
            Connect MetaMask
        </Button>
    );
};
