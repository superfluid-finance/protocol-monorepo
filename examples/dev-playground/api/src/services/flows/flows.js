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
  const owner = sf.user({ address: ownerAddress, token: tokenAddress })
  let flowRate = 0
  const { cfa: { flows: { outFlows } } = {} } = await user.details()
  console.log(outFlows)
}
