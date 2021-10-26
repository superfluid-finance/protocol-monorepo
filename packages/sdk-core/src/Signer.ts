// import { ethers } from "ethers";
// import {
//   Signer as AbstractSigner
// } from '@ethersproject/abstract-signer/lib/index';
// import { FallbackProvider } from "@ethersproject/providers";

// export interface Provider extends AbstractSigner, FallbackProvider {
// 	connection?: Connection;
// 	_network: Network;
// 	call: AbstractSigner['call'] | FallbackProvider['call'];
// 	getBalance: GenericGetBalance;
// 	getTransactionCount: GenericGetTransactionCount;
// 	resolveName: AbstractSigner['resolveName'] | FallbackProvider['resolveName'];
// 	sendTransaction: GenericSendTransaction;
// 	// eslint-disable-next-line @typescript-eslint/no-explicit-any
// 	send?: (method: string, parameters: string[]) => any;
//   }

// /**
//  * The goal of this class is to take in a variety of different providerWithSigner/signers
//  * web3, ethers and return a signer object to execute transactions on the blockchain
//  */
// export default class Signer {
// 	readonly provider: Provider;
// 	constructor(options: any) {
// 		this.provider = _createProvider(options);
// 	}

// }
//  export function _createProvider(options: any = {}) : Provider {
// 	// eslint-disable-next-line @typescript-eslint/no-explicit-any
// 	let provider = options.provider || (options.network || 'mainnet');
// 	const isADefaultProvider = !!ethers.providers.getNetwork(provider.toString());
  
// 	const isObject = typeof provider === 'object';
  
// 	// User passed an ethers.js provider/signer/wallet object
// 	if (isObject && (provider._isSigner || provider._isProvider)) {
// 	  return provider;
// 	}
  
// 	// Create an ethers provider, web3s can sign
// 	if (isADefaultProvider) {
// 	  provider = ethers.getDefaultProvider(provider);
// 	} else if (isObject) {
// 	  provider = new ethers.providers.Web3Provider(provider).getSigner();
// 	} else {
// 	  provider = new ethers.providers.JsonRpcProvider(provider);
// 	}
  
// 	// Add an explicit signer
// 	if (options.privateKey) {
// 	  provider = new ethers.Wallet(options.privateKey, provider);
// 	} else if (options.mnemonic) {
// 	  provider = new ethers.Wallet(ethers.Wallet.fromMnemonic(options.mnemonic), provider);
// 	}
  
// 	return provider;
//   }