import {
  getDefaultWallets
} from '@rainbow-me/rainbowkit';
import {
  chain,
  configureChains,
  createClient
} from 'wagmi';
import { publicProvider } from 'wagmi/providers/public';

export const { chains, provider } = configureChains(
  [chain.polygon, chain.polygonMumbai, chain.goerli],
  [
    publicProvider()
  ]
);

export const { connectors } = getDefaultWallets({
  appName: 'SDK-redux example',
  chains
});

export const wagmiClient = createClient({
  autoConnect: false,
  connectors,
  provider
})
