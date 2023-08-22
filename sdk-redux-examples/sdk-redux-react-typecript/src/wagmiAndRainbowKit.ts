import {
  getDefaultWallets
} from '@rainbow-me/rainbowkit';
import {
  configureChains,
  createClient
} from 'wagmi';
import {
  polygon,
  polygonMumbai,
  goerli
} from 'wagmi/chains'
import { publicProvider } from 'wagmi/providers/public';

export const { chains, provider } = configureChains(
  [polygon, polygonMumbai, goerli],
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
