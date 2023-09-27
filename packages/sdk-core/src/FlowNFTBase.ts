import { ethers } from "ethers";

import ERC721MetadataToken from "./ERC721Token";
import { SFError } from "./SFError";
import { NFTFlowData } from "./interfaces";
import { FlowNFTBase__factory, IFlowNFTBase } from "./typechain-types";
import { normalizeAddress } from "./utils";

export default class FlowNFTBase extends ERC721MetadataToken {
    override readonly contract: IFlowNFTBase;
    constructor(address: string) {
        super(address);
        this.contract = new ethers.Contract(
            address,
            FlowNFTBase__factory.abi
        ) as IFlowNFTBase;
    }

    /** ### ConstantInflowNFT Contract Read Functions ### */

    /**
     * Returns the computed `tokenId` of a flow NFT given a sender and receiver.
     * @param sender the flow sender
     * @param receiver the flow receiver
     * @returns
     */
    getTokenId = async ({
        superToken,
        sender,
        receiver,
        providerOrSigner,
    }: {
        superToken: string;
        sender: string;
        receiver: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        const normalizedSuperToken = normalizeAddress(superToken);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);
        try {
            const tokenId = await this.contract

                .connect(providerOrSigner)
                .getTokenId(
                    normalizedSuperToken,
                    normalizedSender,
                    normalizedReceiver
                );
            return tokenId.toString();
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting token id",
                cause: err,
            });
        }
    };

    /**
     * Returns the NFT flow data of the NFT with `tokenId`.
     * @param tokenId the token id
     * @returns {NFTFlowData} the NFT flow data
     */
    flowDataByTokenId = async ({
        tokenId,
        providerOrSigner,
    }: {
        tokenId: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<NFTFlowData> => {
        try {
            const flowData = await this.contract
                .connect(providerOrSigner)
                .flowDataByTokenId(tokenId);
            return this._sanitizeNFTFlowData(flowData);
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting flow data by token id",
                cause: err,
            });
        }
    };
}
