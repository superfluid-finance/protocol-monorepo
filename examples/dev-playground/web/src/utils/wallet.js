import { Web3Provider } from '@ethersproject/providers'

import { unlockBrowser } from './browser'
import { unlockWalletConnect, isWalletConnect } from './walletConnect'

export const unlockWallet = async (options) => {
  if (isWalletConnect(options)) return unlockWalletConnect(options)
  return unlockBrowser(options)
}
