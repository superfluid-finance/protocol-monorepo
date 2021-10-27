// import { ethers } from "ethers";
// /**
//  * The goal of this class is to take in a variety of different providerWithSigner/signers
//  * web3, ethers and return a signer object to execute transactions on the blockchain
//  */
// export default class Signer {
//     constructor(options: any) {}
// }
// export function _createProvider(options: any = {}) {
//     /**
//      * Option 1: Client-side - We should support new Web3Provider(window.ethereum) or
//      * rawProvider = await web3Modal.connect();
//      * const connectedProvider = new Web3Provider(rawProvider, "any");
//      * Option 2: Server-side - We should take in a ProviderWithSigner, a wallet,
//      * or an ethers or web3.js object in a test environment where the signer is
//      * injected from hardhat.config.ts or truffle.config.js
//      */
//     // // eslint-disable-next-line @typescript-eslint/no-explicit-any
//     // let provider = options.provider || options.network || "mainnet";
//     // const isADefaultProvider = !!ethers.providers.getNetwork(
//     //     provider.toString()
//     // );
//     // const isObject = typeof provider === "object";
//     // // User passed an ethers.js provider/signer/wallet object
//     // if (isObject && (provider._isSigner || provider._isProvider)) {
//     //     return provider;
//     // }
//     // // Create an ethers provider, web3s can sign
//     // if (isADefaultProvider) {
//     //     provider = ethers.getDefaultProvider(provider);
//     // } else if (isObject) {
//     //     provider = new ethers.providers.Web3Provider(provider).getSigner();
//     // } else {
//     //     provider = new ethers.providers.JsonRpcProvider(provider);
//     // }
//     // // Add an explicit signer
//     // if (options.privateKey) {
//     //     provider = new ethers.Wallet(options.privateKey, provider);
//     // } else if (options.mnemonic) {
//     //     provider = new ethers.Wallet(
//     //         ethers.Wallet.fromMnemonic(options.mnemonic),
//     //         provider
//     //     );
//     // }
//     // return provider;
// }
