import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'
import { defaultAbiCoder } from '@ethersproject/abi'
import toast from 'react-hot-toast'

import { getErrorResponse } from './general'
import { unlockWallet } from './wallet'

export const BATCH_STREAM = 'batchStream'
export const BATCH_TRANSFER = 'batchTransfer'

export const batchCall = async ({
  tokenAddress,
  recipients,
  amounts,
  type,
}) => {
  try {
    if (recipients.length !== amounts.length)
      throw Error('The number of recipients and amounts must be the same')
    toast('Connecting to your wallet...')
    const { walletProvider, walletAddress, network } = await unlockWallet({
      debug: true,
      infuraId: process.env.INFURA_ENDPOINT_KEY,
    })
    toast(`Your wallet is on chain ${network.chainId}`)

    const sf = new SuperfluidSDK.Framework({
      ethers: walletProvider,
    })
    await sf.initialize()
    let calls = []
    if (type === BATCH_STREAM) {
      calls = recipients.map((recipient, index) => {
        return [
          201, // OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT
          sf.agreements.cfa.address,
          defaultAbiCoder.encode(
            ['bytes', 'bytes'],
            [
              sf.agreements.cfa.contract.methods
                .createFlow(tokenAddress, recipient, amounts[index], '0x')
                .encodeABI(),
              '0x',
            ]
          ),
        ]
      })
    } else {
      calls = recipients.map((recipient, index) => {
        return [
          2, // OPERATION_TYPE_ERC20_TRANSFER_FROM
          tokenAddress,
          defaultAbiCoder.encode(
            ['address', 'address', 'uint256'],
            [walletAddress, recipient, amounts[index]]
          ),
        ]
      })
    }

    const tx = await sf.host.batchCall(calls)
    // const tx = //
    return { tx }
  } catch (err) {
    console.log(err)
    return {
      ...getErrorResponse(err, 'batchCall'),
    }
  }
}
