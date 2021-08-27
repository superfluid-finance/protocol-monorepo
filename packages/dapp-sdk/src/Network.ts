import {Account} from "./Account";

export interface Network {
    id: number;
    nativeTokenSymbol?: string,
    resolverAddress?: string,
    accounts: { [address: string]: Account }
    streamDetails: { [transactionHash: string]: StreamDetails }
}

export interface StreamCompositeKey {
    networkId: number,
    transactionHash: string
}

export interface StreamDetails extends StreamCompositeKey {
    fromAddress: string,
    toAddress: string,
    isActive: boolean
}
