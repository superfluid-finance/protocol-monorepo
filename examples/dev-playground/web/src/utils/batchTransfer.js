import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'
import { defaultAbiCoder } from '@ethersproject/abi'
import toast from 'react-hot-toast'

import { getErrorResponse } from './general'
import { unlockBrowser } from './connect'

export const batchTransfer = async ({ tokenAddress, recipients, amounts }) => {
  try {
    if (recipients.length !== amounts.length)
      throw Error('The number of recipients and amounts must be the same')
    toast('Connecting to your wallet...')
    const { walletProvider, walletAddress, network } = await unlockBrowser({
      debug: true,
    })
    toast('Your wallet is on ', network)

    const sf = new SuperfluidSDK.Framework({
      ethers: walletProvider,
    })
    await sf.initialize()
    const calls = recipients.map((recipient, index) => {
      return [
        2, // OPERATION_TYPE_ERC20_TRANSFER_FROM
        tokenAddress,
        defaultAbiCoder.encode(
          ['address', 'address', 'uint256'],
          [walletAddress, recipient, amounts[index]]
        ),
      ]
    })
    const tx = await sf.host.batchCall(calls)
    // const tx = //
    return { tx }
  } catch (err) {
    console.log(err)
    return {
      ...getErrorResponse(err, 'batchTransfer'),
    }
  }
}
