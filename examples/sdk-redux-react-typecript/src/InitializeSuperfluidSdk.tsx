import React, { FC, ReactElement } from "react";
import { Framework } from "@superfluid-finance/js-sdk";
import { Web3Provider } from "@ethersproject/providers";
import { Button } from "@mui/material";
import Web3Modal from "web3modal";
import { superfluidFrameworkSource } from "./redux/store";

interface Props {
    onSuperfluidSdkInitialized: (sf: Framework) => void;
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

        const provider = await web3Modal.connect();
        const ethers = new Web3Provider(provider);

        const superfluidSdk = new Framework({
            ethers: ethers,
        });
        await superfluidSdk.initialize();

        superfluidFrameworkSource.setForReadAndWrite(
            5,
            Promise.resolve(superfluidSdk)
        );
        onSuperfluidSdkInitialized(superfluidSdk);
    };

    return (
        <Button variant="contained" onClick={handleWallet}>
            Connect MetaMask
        </Button>
    );
};
