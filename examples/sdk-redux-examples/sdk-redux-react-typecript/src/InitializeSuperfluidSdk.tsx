import React, { FC, ReactElement } from "react";
import { Framework } from "@superfluid-finance/sdk-core";
import { Web3Provider } from "@ethersproject/providers";
import { Button } from "@mui/material";
import { ethers } from "ethers";
import Web3Modal from "web3modal";
import { setFrameworkForSdkRedux, setSignerForSdkRedux } from "@superfluid-finance/sdk-redux";

interface Props {
    onSuperfluidSdkInitialized: (sf: Framework, provider: Web3Provider) => void;
}

export const chainIds = [
    3, // ROPSTEN
    4, // RINKEBY
    5, // GOERLI
    42, // KOVAN
    // 100, // XDAI //TODO(KK): No infura support
    137, // MATIC
    80001 // MUMBAI
];

export const InitializeSuperfluidSdk: FC<Props> = ({
                                                       onSuperfluidSdkInitialized
                                                   }): ReactElement => {
    const handleWallet = async () => {
        const providerOptions = {
            /* See Provider Options Section */
        };

        // Configure Infura Providers when environment variable is present.
        const infuraProviders = !!process.env.REACT_APP_INFURA_ID
            ? chainIds.map((chainId) => ({
                chainId,
                frameworkGetter: () =>
                    Framework.create({
                        chainId,
                        provider: new ethers.providers.InfuraProvider(
                            chainId,
                            process.env.REACT_APP_INFURA_ID
                        )
                    })
            }))
            : [];

        infuraProviders.map((x) =>
            setFrameworkForSdkRedux(x.chainId, x.frameworkGetter)
        );

        const web3Modal = new Web3Modal({
            cacheProvider: false,
            providerOptions
        });

        const web3ModalProvider = await web3Modal.connect();

        let ethersWeb3Provider = new Web3Provider(web3ModalProvider);
        let currentNetwork = await ethersWeb3Provider.getNetwork();

        const superfluidSdk = await Framework.create({
            chainId: currentNetwork.chainId,
            provider: ethersWeb3Provider
        });

        // Set active provider & signer from MetaMask
        setFrameworkForSdkRedux(currentNetwork.chainId, superfluidSdk);
        setSignerForSdkRedux(currentNetwork.chainId, ethersWeb3Provider.getSigner());

        onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);

        web3ModalProvider.on("accountsChanged", (accounts: string[]) => {
            setSignerForSdkRedux(
                currentNetwork.chainId,
                ethersWeb3Provider.getSigner()
            );

            onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);
        });

        web3ModalProvider.on("chainChanged", async (chainId: number) => {
            const parsedChainId = Number(chainId);

            ethersWeb3Provider = new Web3Provider(web3ModalProvider);
            currentNetwork = await ethersWeb3Provider.getNetwork();

            const newSdk = await Framework.create({
                chainId: parsedChainId,
                provider: ethersWeb3Provider
            });

            // Re-set INFURA providers
            infuraProviders.map((x) =>
                setFrameworkForSdkRedux(x.chainId, x.frameworkGetter)
            );

            // Set active provider & signer from MetaMask
            setFrameworkForSdkRedux(parsedChainId, newSdk);
            setSignerForSdkRedux(parsedChainId, ethersWeb3Provider.getSigner());

            onSuperfluidSdkInitialized(newSdk, ethersWeb3Provider);
        });
    };

    return (
        <Button variant="contained" onClick={handleWallet}>
            Connect MetaMask
        </Button>
    );
};
