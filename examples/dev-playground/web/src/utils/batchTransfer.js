import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'

import toast from 'react-hot-toast'

import { getErrorResponse } from './general'
import { unlockBrowser } from './connect'

export const batchTransfer = async ({ tokenAddress, recipients, amounts }) => {
  try {
    toast('Connecting to your wallet...')
    const { walletProvider, walletAddress, network } = await unlockBrowser({
      debug: true,
    })
    toast('Your wallet is on ', network)

    const sf = new SuperfluidSDK.Framework({
      ethers: walletProvider,
    })
    await sf.initialize()
    // const tx = //
    return { tx }
  } catch (err) {
    console.log(err)
    return {
      ...getErrorResponse(err, 'donateFlow'),
    }
  }
}
