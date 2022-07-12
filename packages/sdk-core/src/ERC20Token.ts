import { ethers } from "ethers";

import Operation from "./Operation";
import { SFError } from "./SFError";
import ERC20WithTokenInfoABI from "./abi/ERC20WithTokenInfo.json";
import { IBaseSuperTokenParams, ITransferFromParams } from "./interfaces";
import { ERC20WithTokenInfo } from "./typechain/ERC20WithTokenInfo";
import { normalizeAddress } from "./utils";

export default class ERC20Token {
    readonly address: string;
    readonly contract: ERC20WithTokenInfo;

    constructor(address: string) {
        this.address = address;
        this.contract = new ethers.Contract(
            this.address,
            ERC20WithTokenInfoABI.abi
        ) as ERC20WithTokenInfo;
    }

    /** ### ERC20 Token Contract Read Functions ### */

    /**
     * Returns the allowance the `owner` has granted the `spender`.
     * @param owner the owner who has allotted the allowance
     * @param spender the spender who has received the allowance
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<string>} the allowance amount
     */
    allowance = async ({
        owner,
        spender,
        providerOrSigner,
    }: {
        owner: string;
        spender: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        const normalizedOwner = normalizeAddress(owner);
        const normalizedSpender = normalizeAddress(spender);
        try {
            const allowance = await this.contract
                .connect(providerOrSigner)
                .allowance(normalizedOwner, normalizedSpender);
            return allowance.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                message: "There was an error getting allowance",
                cause: err,
            });
        }
    };

    /**
     * Returns the ERC20 balanceOf the `account`, this can't be negative and will just display 0.
     * @param account the account you would like to query
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<string>} the token balance of `account`
     */
    balanceOf = async ({
        account,
        providerOrSigner,
    }: {
        account: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const normalizedAccount = normalizeAddress(account);
            const balanceOf = await this.contract
                .connect(providerOrSigner)
                .balanceOf(normalizedAccount);
            return balanceOf.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                message: "There was an error getting balanceOf",
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
                type: "SUPERTOKEN_READ",
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
                type: "SUPERTOKEN_READ",
                message: "There was an error getting symbol",
                cause: err,
            });
        }
    };

    /**
     * Returns the total supply of the token.
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<string>} the total supply of the token
     */
    totalSupply = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const totalSupply = await this.contract
                .connect(providerOrSigner)
                .totalSupply();
            return totalSupply.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                message: "There was an error getting totalSupply",
                cause: err,
            });
        }
    };

    /** ### ERC20 Token Contract Write Functions ### */

    /**
     * Approve `receiver` to spend `amount` tokens.
     * @param receiver The receiver approved.
     * @param amount The amount approved.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approve = (params: IBaseSuperTokenParams): Operation => {
        const normalizedReceiver = normalizeAddress(params.receiver);
        const txn = this.contract.populateTransaction.approve(
            normalizedReceiver,
            params.amount,
            params.overrides || {}
        );
        return new Operation(txn, "ERC20_APPROVE");
    };

    /**
     * Transfer `receiver` `amount` tokens.
     * @param receiver The receiver of the transfer.
     * @param amount The amount to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    transfer = (params: IBaseSuperTokenParams): Operation => {
        const normalizedReceiver = normalizeAddress(params.receiver);
        const txn = this.contract.populateTransaction.transfer(
            normalizedReceiver,
            params.amount,
            params.overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Transfer from `sender` to `receiver` `amount` tokens.
     * @param sender The sender of the transfer.
     * @param receiver The receiver of the transfer.
     * @param amount The amount to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    transferFrom = (params: ITransferFromParams): Operation => {
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const txn = this.contract.populateTransaction.transferFrom(
            normalizedSender,
            normalizedReceiver,
            params.amount,
            params.overrides || {}
        );
        return new Operation(txn, "ERC20_TRANSFER_FROM");
    };
}
