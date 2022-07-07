import { FC, ReactElement, useCallback, useEffect } from "react";
import { Framework } from "@superfluid-finance/sdk-core";
import { JsonRpcProvider, Web3Provider } from "@ethersproject/providers";
import { Button, Stack } from "@mui/material";
import { ethers, Signer } from "ethers";
import Web3Modal from "web3modal";
import { setFrameworkForSdkRedux } from "@superfluid-finance/sdk-redux";
import WalletConnectProvider from "@walletconnect/web3-provider";
import { chains } from "./wagmiAndRainbowKit";
import { ConnectButton } from "@rainbow-me/rainbowkit";
import { useNetwork, useProvider, useSigner } from "wagmi";

interface Props {
    onSuperfluidSdkInitialized: (
        sf: Framework,
        provider: ethers.providers.Provider,
        signer: Signer
    ) => void;
}

// Enter a valid infura key here to avoid being rate limited
// You can get a key for free at https://infura.io/register
const INFURA_ID = "90db0e566ddf4de7b05ed1e57306bbe4";

export const InitializeSuperfluidSdk: FC<Props> = ({
    onSuperfluidSdkInitialized,
}): ReactElement => {
    const setInfuraProviders = useCallback(() => {
        // Configure Infura Providers when environment variable is present.
        const infuraProviders = !!process.env.REACT_APP_INFURA_ID
            ? chains.map(({ id: chainId }) => ({
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
    }, []);

    const handleWeb3Modal = async () => {
        const providerOptions = {
            walletconnect: {
                package: WalletConnectProvider,
                options: {
                    infuraId: INFURA_ID,
                },
            },
        };

        setInfuraProviders();

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

        onSuperfluidSdkInitialized(
            superfluidSdk,
            ethersWeb3Provider,
            await ethersWeb3Provider.getSigner()
        );

        web3ModalProvider.on("accountsChanged", async (accounts: string[]) => {
            onSuperfluidSdkInitialized(
                superfluidSdk,
                ethersWeb3Provider,
                await ethersWeb3Provider.getSigner()
            );
        });

        web3ModalProvider.on("chainChanged", async (chainId: number) => {
            const parsedChainId = Number(chainId);

            ethersWeb3Provider = new Web3Provider(web3ModalProvider);
            currentNetwork = await ethersWeb3Provider.getNetwork();

            const newSdk = await Framework.create({
                chainId: parsedChainId,
                provider: ethersWeb3Provider,
            });

            setInfuraProviders();

            // Set active provider & signer from MetaMask
            setFrameworkForSdkRedux(parsedChainId, newSdk);

            onSuperfluidSdkInitialized(
                newSdk,
                ethersWeb3Provider,
                await ethersWeb3Provider.getSigner()
            );
        });
    };

    const { chain } = useNetwork();
    const { data: signer } = useSigner();
    const provider = useProvider();

    useEffect(() => {
        if (chain && signer && provider) {
            setInfuraProviders();
            Framework.create({
                chainId: chain.id,
                provider: provider,
            }).then((sdk) => {
                setFrameworkForSdkRedux(chain.id, sdk);
                onSuperfluidSdkInitialized(sdk, provider, signer);
            });
        }
    }, [chain, signer, provider]);

    return (
        <Stack direction="row" gap={2}>
            <Button variant="contained" onClick={handleWeb3Modal}>
                Connect with Web3Modal
            </Button>
            <ConnectButton label="Connect with RainbowKit" />
        </Stack>
    );
};
