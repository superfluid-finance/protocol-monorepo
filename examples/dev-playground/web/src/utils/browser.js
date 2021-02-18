import { Web3Provider } from '@ethersproject/providers'

import { getErrorResponse } from './general'

export const isWeb3EnabledBrowser = () =>
  typeof window !== 'undefined' && typeof window.ethereum !== 'undefined'

export const unlockBrowser = async ({ debug }) => {
  try {
    if (!isWeb3EnabledBrowser()) {
      return { hasWallet: false, isUnlocked: false }
    }
    window.ethereum.autoRefreshOnNetworkChange = false

    const walletAddress = await window.ethereum.request({
      method: 'eth_requestAccounts',
      params: [
        {
          eth_accounts: {},
        },
      ],
    })

    const walletProvider = new Web3Provider(window.ethereum)

    const network = await walletProvider.getNetwork()
    if (debug)
      /* eslint-disable-next-line no-console */
      console.log(
        'Web3Browser wallet loaded: ',
        JSON.stringify({ walletAddress, network })
      )

    return {
      hasWallet: true,
      isUnlocked: true,
      walletAddress: walletAddress[0],
      network,
      walletProvider,
    }
  } catch (error) {
    if (isWeb3EnabledBrowser()) {
      if (debug)
        /* eslint-disable-next-line no-console */
        console.log('Web3 detected in browser, but wallet unlock failed')
      return {
        hasWallet: true,
        isUnlocked: false,
        ...getErrorResponse(error, 'unlockBrowser'),
      }
    }
    return {
      hasWallet: false,
      isUnlocked: false,
      ...getErrorResponse(error, 'unlockBrowser'),
    }
  }
}
