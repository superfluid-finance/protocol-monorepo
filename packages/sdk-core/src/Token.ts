import { ethers } from "ethers";

import Operation from "./Operation";
import { SFError } from "./SFError";
import ERC20WithTokenInfoABI from "./abi/ERC20WithTokenInfo.json";
import { IBaseSuperTokenParams, ITransferFromParams } from "./interfaces";
import { ERC20WithTokenInfo } from "./typechain/ERC20WithTokenInfo";
import { normalizeAddress } from "./utils";

export default class Token {
    readonly address: string;

    constructor(address: string) {
        this.address = address;
    }

    private get tokenContract() {
        return new ethers.Contract(
            this.address,
            ERC20WithTokenInfoABI.abi
        ) as ERC20WithTokenInfo;
    }

    // ERC20 Token Contract Read Functions

    /**
     * @dev Returns the allowance the `owner` has granted the `spender`.
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
            const allowance = await this.tokenContract
                .connect(providerOrSigner)
                .allowance(normalizedOwner, normalizedSpender);
            return allowance.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting allowance",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Returns the ERC20 balanceOf the `account`, this can't be negative and will just display 0.
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
            const balanceOf = await this.tokenContract
                .connect(providerOrSigner)
                .balanceOf(normalizedAccount);
            return balanceOf.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting balanceOf",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Returns the token name
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {string} the token name
     */
    name = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const name = await this.tokenContract
                .connect(providerOrSigner)
                .name();
            return name;
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting name",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Returns the token symbol
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {string} the token symbol
     */
    symbol = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const symbol = await this.tokenContract
                .connect(providerOrSigner)
                .symbol();
            return symbol;
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting symbol",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Returns the total supply of the token.
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<string>} the total supply of the token
     */
    totalSupply = async ({
        providerOrSigner,
    }: {
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }): Promise<string> => {
        try {
            const totalSupply = await this.tokenContract
                .connect(providerOrSigner)
                .totalSupply();
            return totalSupply.toString();
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting totalSupply",
                errorObject: err,
            });
        }
    };

    // ERC20 Token Contract Write Functions

    /**
     * @dev Approve `receiver` to spend `amount` tokens.
     * @param receiver The receiver approved.
     * @param amount The amount approved.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approve = ({
        receiver,
        amount,
        overrides,
    }: IBaseSuperTokenParams): Operation => {
        const normalizedReceiver = normalizeAddress(receiver);
        const txn = this.tokenContract.populateTransaction.approve(
            normalizedReceiver,
            amount,
            overrides || {}
        );
        return new Operation(txn, "ERC20_APPROVE");
    };

    /**
     * @dev Transfer `receiver` `amount` tokens.
     * @param receiver The receiver of the transfer.
     * @param amount The amount to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    transfer = ({
        receiver,
        amount,
        overrides,
    }: IBaseSuperTokenParams): Operation => {
        const normalizedReceiver = normalizeAddress(receiver);
        const txn = this.tokenContract.populateTransaction.transfer(
            normalizedReceiver,
            amount,
            overrides || {}
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * @dev Transfer from `sender` to `receiver` `amount` tokens.
     * @param sender The sender of the transfer.
     * @param receiver The receiver of the transfer.
     * @param amount The amount to be transferred.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    transferFrom = ({
        sender,
        receiver,
        amount,
        overrides,
    }: ITransferFromParams): Operation => {
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);
        const txn = this.tokenContract.populateTransaction.transferFrom(
            normalizedSender,
            normalizedReceiver,
            amount,
            overrides || {}
        );
        return new Operation(txn, "ERC20_TRANSFER_FROM");
    };
}
