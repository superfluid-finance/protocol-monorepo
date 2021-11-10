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

        const superfluidSdk = await Framework.create({
            chainId: 5,
            provider: ethersWeb3Provider as any, // TODO(KK): as any
        });

        // @ts-ignore
        window.sf = superfluidSdk;

        superfluidFrameworkSource.setFramework(
            5,
            Promise.resolve(superfluidSdk)
        );
        superfluidFrameworkSource.setSigner(
            5,
            Promise.resolve(ethersWeb3Provider.getSigner() as any) // TODO(KK): as any
        );

        onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);
    };

    return (
        <Button variant="contained" onClick={handleWallet}>
            Connect MetaMask
        </Button>
    );
};
