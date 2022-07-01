import { FC, ReactElement } from "react";
import { Framework } from "@superfluid-finance/sdk-core";
import { Web3Provider } from "@ethersproject/providers";
import { Button } from "@mui/material";
import { ethers } from "ethers";
import Web3Modal from "web3modal";
import { setFrameworkForSdkRedux } from "@superfluid-finance/sdk-redux";
import WalletConnectProvider from "@walletconnect/web3-provider";

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
    80001, // MUMBAI
];

// Enter a valid infura key here to avoid being rate limited
// You can get a key for free at https://infura.io/register
const INFURA_ID = "90db0e566ddf4de7b05ed1e57306bbe4";

export const InitializeSuperfluidSdk: FC<Props> = ({
    onSuperfluidSdkInitialized,
}): ReactElement => {
    const handleWallet = async () => {
        const providerOptions = {
            walletconnect: {
                package: WalletConnectProvider,
                options: {
                    infuraId: INFURA_ID,
                },
            },
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
                          ),
                      }),
              }))
            : [];

        infuraProviders.map((x) =>
            setFrameworkForSdkRedux(x.chainId, x.frameworkGetter)
        );

        const web3Modal = new Web3Modal({
            cacheProvider: false,
            providerOptions,
        });

        const web3ModalProvider = await web3Modal.connect();

        let ethersWeb3Provider = new Web3Provider(web3ModalProvider);
        let currentNetwork = await ethersWeb3Provider.getNetwork();

        const superfluidSdk = await Framework.create({
            chainId: currentNetwork.chainId,
            provider: ethersWeb3Provider,
        });

        // Set active provider & signer from MetaMask
        setFrameworkForSdkRedux(currentNetwork.chainId, superfluidSdk);

        onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);

        web3ModalProvider.on("accountsChanged", (accounts: string[]) => {
            onSuperfluidSdkInitialized(superfluidSdk, ethersWeb3Provider);
        });

        web3ModalProvider.on("chainChanged", async (chainId: number) => {
            const parsedChainId = Number(chainId);

            ethersWeb3Provider = new Web3Provider(web3ModalProvider);
            currentNetwork = await ethersWeb3Provider.getNetwork();

            const newSdk = await Framework.create({
                chainId: parsedChainId,
                provider: ethersWeb3Provider,
            });

            // Re-set INFURA providers
            infuraProviders.map((x) =>
                setFrameworkForSdkRedux(x.chainId, x.frameworkGetter)
            );

            // Set active provider & signer from MetaMask
            setFrameworkForSdkRedux(parsedChainId, newSdk);

            onSuperfluidSdkInitialized(newSdk, ethersWeb3Provider);
        });
    };

    return (
        <Button variant="contained" onClick={handleWallet}>
            Connect MetaMask
        </Button>
    );
};
