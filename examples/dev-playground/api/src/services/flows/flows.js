import { db } from 'src/lib/db'
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { InfuraProvider } from '@ethersproject/providers'

const sf = new SuperfluidSDK.Framework({
  ethers: new InfuraProvider('goerli', process.env.INFURA_ENDPOINT_KEY),
})

export const flow = async ({
  recipientAddress,
  ownerAddress,
  tokenAddress,
}) => {
  await sf.initialize()
  const flow = await sf.cfa.getFlow({
    sender: ownerAddress,
    superToken: tokenAddress,
    receiver: recipientAddress,
  })
  return {
    flowRate: flow.flowRate,
    recipientAddress,
    tokenAddress,
    ownerAddress,
  }
}
