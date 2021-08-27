export interface AccountCompositeKey {
    accountAddress: string; // TODO: Make hash a value object. Or use types from Ethers.js?
    networkId: number;
}

export interface Account extends AccountCompositeKey {
    activeStreams?: Array<StreamIndex>,
    inactiveStreams?: Array<StreamIndex>
}

export interface StreamIndex {
    transactionHash: string
}
