import { Web3Provider } from '@ethersproject/providers'
import WalletConnectProvider from '@walletconnect/web3-provider'

import { getErrorResponse } from './general'
const WALLET_CONNECT_LOCAL_KEY = 'walletconnect'

export const disconnectWalletConnect = () => {
  localStorage.removeItem(WALLET_CONNECT_LOCAL_KEY)
}

export const unlockWalletConnect = async ({
  debug,
  onNetworkChange,
  onDisconnect,
}) => {
  try {
    const provider = new WalletConnectProvider({
      rpc: {
        100: process.env.XDAI_RPC_URL,
      },
      infuraId: process.env.INFURA_ENDPOINT_KEY,
    })
    console.log(provider)
    // provider.on('chainChanged', onNetworkChange)
    provider.on('close', () => {
      disconnectWalletConnect()
      onDisconnect && onDisconnect()
    })

    await provider.enable()
    const walletProvider = new Web3Provider(provider)
    const network = await walletProvider.getNetwork()
    const walletAddress = provider.accounts[0]
    if (debug)
      /* eslint-disable-next-line no-console */
      console.log(
        'WalletConnect wallet loaded: ',
        JSON.stringify({ walletAddress, network })
      )
    return {
      hasWallet: true,
      isUnlocked: true,
      walletAddress,
      network,
      walletProvider,
    }
  } catch (error) {
    return {
      hasWallet: false,
      isUnlocked: false,
      ...getErrorResponse(error, 'unlockWalletConnect'),
    }
  }
}

export const isWalletConnect = () => {
  try {
    return !!localStorage.getItem(WALLET_CONNECT_LOCAL_KEY)
  } catch (error) {
    /* eslint-disable-next-line no-console */
    if (debug) console.log('Issue during isWalletConnect():', error)
    return false
  }
}
