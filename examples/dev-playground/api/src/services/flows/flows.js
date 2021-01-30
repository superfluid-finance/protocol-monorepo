import { db } from 'src/lib/db'
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { InfuraProvider } from '@ethersproject/providers'

const provider = new InfuraProvider('goerli', process.env.INFURA_ENDPOINT_KEY)
const sf = new SuperfluidSDK.Framework({
  version: 'v1', // Protocol release version
  web3Provider: provider, // your web3 provider
})

export const flow = async ({
  recipientAddress,
  ownerAddress,
  tokenAddress,
}) => {
  await sf.initialize()
  const owner = sf.user({ address: ownerAddress, token: tokenAddress })
  let flowRate = 0
  const { cfa: { flows: { outFlows } } = {} } = await user.details()
}
