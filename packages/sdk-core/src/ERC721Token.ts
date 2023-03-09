import {
    IERC721Metadata,
    IERC721Metadata__factory,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ethers } from "ethers";

import Operation from "./Operation";
import { SFError } from "./SFError";
import {
    NFTApproveParams,
    NFTFlowData,
    NFTSetApprovalForAllParams,
    SafeTransferFromParams,
    TransferFromParams,
} from "./interfaces";
import { getSanitizedTimestamp, normalizeAddress } from "./utils";

export default class ERC721MetadataToken {
    readonly address: string;
    readonly contract: IERC721Metadata;

    constructor(address: string) {
        this.address = address;

        this.contract = new ethers.Contract(
            address,
            IERC721Metadata__factory.abi
        ) as IERC721Metadata;
    }

    /** ### ERC721 Token Contract Read Functions ### */

    /**
     * Returns the ERC721 balanceOf the `owner`.
     * @param owner the owner you would like to query
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<string>} the token balance of `owner`
     */
    balanceOf = async ({
        owner,
        providerOrSigner,
    }: {
        owner: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const normalizedOwner = normalizeAddress(owner);
            const balanceOf = await this.contract
                .connect(providerOrSigner)
                .balanceOf(normalizedOwner);
            return balanceOf.toString();
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting balanceOf",
                cause: err,
            });
        }
    };

    /**
     * Returns the owner of the NFT specified by `tokenId`.
     * NOTE: Throws if `tokenId` is not a valid NFT.
     * @param tokenId the token id
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {string} the address of the owner of the NFT
     */
    ownerOf = async ({
        tokenId,
        providerOrSigner,
    }: {
        tokenId: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const ownerOf = await this.contract
                .connect(providerOrSigner)
                .ownerOf(tokenId);
            return ownerOf.toString();
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting ownerOf",
                cause: err,
            });
        }
    };

    /**
     * Returns the approved address for a single NFT, or the zero address if there is none.
     * @param tokenId the token id
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {bool}
     */
    getApproved = async ({
        tokenId,
        providerOrSigner,
    }: {
        tokenId: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const approved = await this.contract

                .connect(providerOrSigner)
                .getApproved(tokenId);
            return approved.toString();
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting getApproved",
                cause: err,
            });
        }
    };

    /**
     * Returns whether `operator` is approved for all of `owner`'s NFTs.
     * @param owner the owner of NFTs
     * @param operator an operator for the owner's NFTs
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {bool}
     */
    isApprovedForAll = async ({
        owner,
        operator,
        providerOrSigner,
    }: {
        owner: string;
        operator: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<boolean> => {
        try {
            const normalizedOwner = normalizeAddress(owner);
            const normalizedOperator = normalizeAddress(operator);
            const approved = await this.contract

                .connect(providerOrSigner)
                .isApprovedForAll(normalizedOwner, normalizedOperator);
            return approved;
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting isApprovedForAll",
                cause: err,
            });
        }
    };

    /**
     * Returns the token name
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {string} the token name
     */
    name = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const name = await this.contract.connect(providerOrSigner).name();
            return name;
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting name",
                cause: err,
            });
        }
    };

    /**
     * Returns the token symbol
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {string} the token symbol
     */
    symbol = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const symbol = await this.contract
                .connect(providerOrSigner)
                .symbol();
            return symbol;
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting symbol",
                cause: err,
            });
        }
    };

    /**
     * Returns the token URI
     * @param tokenId the token id
     * @returns {string}
     */
    tokenURI = async ({
        tokenId,
        providerOrSigner,
    }: {
        tokenId: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const uri = await this.contract

                .connect(providerOrSigner)
                .tokenURI(tokenId);
            return uri;
        } catch (err) {
            throw new SFError({
                type: "NFT_READ",
                message: "There was an error getting tokenURI",
                cause: err,
            });
        }
    };

    /** ### ERC721 Token Contract Write Functions ### */

    /**
     * Approve `approved` to spend `tokenId` NFT.
     * @param approved The receiver approved.
     * @param tokenId The tokenId approved.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    approve = (params: NFTApproveParams): Operation => {
        const normalizedReceiver = normalizeAddress(params.approved);
        const txn = this.contract.populateTransaction.approve(
            normalizedReceiver,
            params.tokenId,
            params.overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Approve `operator` to spend all NFTs of the signer (`msg.sender`).
     * @param operator The operator approved.
     * @param approved The approved status.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    setApprovalForAll = (params: NFTSetApprovalForAllParams): Operation => {
        const normalizedOperator = normalizeAddress(params.operator);
        const txn = this.contract.populateTransaction.setApprovalForAll(
            normalizedOperator,
            params.approved,
            params.overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Transfer `tokenId` from `from` to `to` .
     * @param from The owner of the NFT.
     * @param to The receiver of the NFT.
     * @param tokenId The token to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    transferFrom = (params: TransferFromParams): Operation => {
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedTo = normalizeAddress(params.to);
        const txn = this.contract.populateTransaction.transferFrom(
            normalizedFrom,
            normalizedTo,
            params.tokenId,
            params.overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Safe transfer `tokenId` from `from` to `to` (see IERC721.sol OZ Natspec for more details).
     * Data is empty in this version of safeTransferFrom.
     * @param from The owner of the NFT.
     * @param to The receiver of the NFT.
     * @param tokenId The token to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    safeTransferFrom = (params: TransferFromParams): Operation => {
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedTo = normalizeAddress(params.to);
        const txn = this.contract.populateTransaction[
            "safeTransferFrom(address,address,uint256)"
        ](normalizedFrom, normalizedTo, params.tokenId, params.overrides || {});
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Safe transfer `tokenId` from `from` to `to` with `data`.
     * @param from The owner of the NFT.
     * @param to The receiver of the NFT.
     * @param tokenId The token to be transferred.
     * @param data The data to be sent with the safe transfer check.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    safeTransferFromWithData = (params: SafeTransferFromParams): Operation => {
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedTo = normalizeAddress(params.to);
        const txn = this.contract.populateTransaction[
            "safeTransferFrom(address,address,uint256,bytes)"
        ](
            normalizedFrom,
            normalizedTo,
            params.tokenId,
            params.data,
            params.overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Sanitizes NFTFlowData, converting number to Date.
     * @param params NFTFlowData
     * @returns {NFTFlowData} sanitized NFTFlowData
     */
    _sanitizeNFTFlowData = (params: {
        flowSender: string;
        flowStartDate: number;
        flowReceiver: string;
    }): NFTFlowData => {
        return {
            flowSender: params.flowSender,
            flowStartDate: getSanitizedTimestamp(params.flowStartDate),
            flowReceiver: params.flowReceiver,
        };
    };
}
