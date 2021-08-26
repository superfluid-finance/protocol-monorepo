import { createDAppSDK } from "dapp-sdk";
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'

const windowWeb3 = window as any

const superfluidSdk = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(windowWeb3.ethereum)
})

export default createDAppSDK(superfluidSdk);
