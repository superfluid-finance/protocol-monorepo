import { ethers } from "ethers";
import { networkNameToChainIdMap } from "./constants";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { getNetworkName } from "./frameworkHelpers";
import { SuperToken as ISuperToken } from "./typechain";
import {
    IConfig,
    ISuperTokenBaseIDAParams,
    ISuperTokenBaseSubscriptionParams,
    ISuperTokenCreateFlowParams,
    ISuperTokenDeleteFlowParams,
    ISuperTokenDistributeParams,
    ISuperTokenUpdateFlowParams,
    ISuperTokenUpdateIndexValueParams,
    ISuperTokenUpdateSubscriptionUnitsParams,
} from "./interfaces";
import Operation from "./Operation";
import { handleError } from "./errorHelper";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";

export interface ITokenConstructorOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId?: number;
    readonly networkName?: string;
}
export interface ITokenOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId: number;
    readonly networkName: string;
}

/**
 * @dev SuperToken Helper Class
 * @description A helper class to create `SuperToken` objects which can interact with the `SuperToken` contract as well as the CFAV1 and IDAV1 contracts of the desired `SuperToken`.
 */
export default class SuperToken {
    readonly options: ITokenOptions;
    readonly cfaV1: ConstantFlowAgreementV1;
    readonly idaV1: InstantDistributionAgreementV1;

    constructor(options: ITokenConstructorOptions) {
        if (!options.chainId && !options.networkName) {
            handleError(
                "SUPERTOKEN_INITIALIZATION",
                "You must input chainId or networkName."
            );
        }
        const networkName = getNetworkName(options);
        this.options = {
            address: options.address,
            chainId:
                options.chainId || networkNameToChainIdMap.get(networkName)!,
            config: options.config,
            networkName,
        };
        this.cfaV1 = new ConstantFlowAgreementV1({
            config: this.options.config,
        });
        this.idaV1 = new InstantDistributionAgreementV1({
            config: this.options.config,
        });
    }

    private get superTokenContract() {
        return new ethers.Contract(
            this.options.address,
            SuperTokenABI
        ) as ISuperToken;
    }

    // SuperToken Contract Read Functions
    /**
     * @dev Returns the token balance of `address`.
     * @param address the target address
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns SuperToken balance
     */
    balanceOf = async (
        address: string,
        providerOrSigner: ethers.providers.Provider | ethers.Signer
    ) => {
        try {
            const balanceOf = await this.superTokenContract
                .connect(providerOrSigner)
                .balanceOf(address);
            return balanceOf;
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting balanceOf",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Returns the real time balance of `address`.
     * @param address the target address
     * @param timestamp the timestamp you'd like to see the data
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns [the available balance, deposit, owed deposit amount]
     */
    realtimeBalanceOf = async (
        address: string,
        timestamp: string,
        providerOrSigner: ethers.providers.Provider | ethers.Signer
    ) => {
        try {
            const realtimeBalanceOf = await this.superTokenContract
                .connect(providerOrSigner)
                .realtimeBalanceOf(address, timestamp);
            return realtimeBalanceOf.availableBalance;
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOf",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Returns the real time balance of `address`.
     * @param address the target address
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns [the available balance, deposit, owed deposit amount, timestamp]
     */
    realtimeBalanceOfNow = async (
        address: string,
        providerOrSigner: ethers.providers.Provider | ethers.Signer
    ) => {
        try {
            const realtimeBalanceOfNow = await this.superTokenContract
                .connect(providerOrSigner)
                .realtimeBalanceOfNow(address);
            return realtimeBalanceOfNow.availableBalance;
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOfNow",
                JSON.stringify(err)
            );
        }
    };

    // SuperToken Contract Write Functions
    /**
     * @dev Approve `recipient` to spend `amount` tokens.
     * @param recipient The recipient approved.
     * @param amount The amount approved.
     * @returns An instance of Operation which can be executed or batched.
     */
    approve = (recipient: string, amount: string): Operation => {
        try {
            const txn = this.superTokenContract.populateTransaction.approve(
                recipient,
                amount
            );
            return new Operation(txn, "ERC20_APPROVE");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Downgrade `amount` SuperToken's.
     * @param amount The amount to be downgraded.
     * @returns An instance of Operation which can be executed or batched.
     */
    downgrade = (amount: string): Operation => {
        try {
            const txn =
                this.superTokenContract.populateTransaction.downgrade(amount);
            return new Operation(txn, "SUPERTOKEN_DOWNGRADE");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Transfer `recipient` `amount` tokens.
     * @param recipient The recipient of the transfer.
     * @param amount The amount to be transferred.
     * @returns An instance of Operation which can be executed or batched.
     */
    transfer = (recipient: string, amount: string): Operation => {
        try {
            const txn = this.superTokenContract.populateTransaction.transfer(
                recipient,
                amount
            );
            return new Operation(txn, "UNSUPPORTED");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Transfer from `sender` to `recipient` `amount` tokens.
     * @param sender The sender of the transfer.
     * @param recipient The recipient of the transfer.
     * @param amount The amount to be transferred.
     * @returns An instance of Operation which can be executed or batched.
     */
    transferFrom = (
        sender: string,
        recipient: string,
        amount: string
    ): Operation => {
        try {
            const txn =
                this.superTokenContract.populateTransaction.transferFrom(
                    sender,
                    recipient,
                    amount
                );
            return new Operation(txn, "ERC20_TRANSFER_FROM");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Upgrade `amount` SuperToken's.
     * @param amount The amount to be upgraded.
     * @returns An instance of Operation which can be executed or batched.
     */
    upgrade = (amount: string): Operation => {
        try {
            const txn =
                this.superTokenContract.populateTransaction.upgrade(amount);
            return new Operation(txn, "SUPERTOKEN_UPGRADE");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    // CFA Functions
    /**
     * @dev Create a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param flowRate The specified flow rate.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ISuperTokenCreateFlowParams): Operation => {
        return this.cfaV1.createFlow({
            flowRate,
            receiver,
            sender,
            superToken: this.options.address,
            userData,
        });
    };

    /**
     * @dev Update a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param flowRate The specified flow rate.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ISuperTokenUpdateFlowParams): Operation => {
        return this.cfaV1.updateFlow({
            flowRate,
            receiver,
            sender,
            superToken: this.options.address,
            userData,
        });
    };

    /**
     * @dev Delete a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = ({
        sender,
        receiver,
        userData,
    }: ISuperTokenDeleteFlowParams): Operation => {
        return this.cfaV1.deleteFlow({
            superToken: this.options.address,
            sender,
            receiver,
            userData,
        });
    };

    // IDA Functions
    /**
     * @dev Creates an IDA Index.
     * @param indexId The id of the index.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = ({
        indexId,
        userData,
    }: ISuperTokenBaseIDAParams): Operation => {
        return this.idaV1.createIndex({
            indexId,
            superToken: this.options.address,
            userData,
        });
    };

    /**
     * @dev Distributes `amount` of token to an index
     * @param indexId The id of the index.
     * @param amount The amount of tokens to be distributed.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = ({
        indexId,
        amount,
        userData,
    }: ISuperTokenDistributeParams): Operation => {
        return this.idaV1.distribute({
            indexId,
            amount,
            superToken: this.options.address,
            userData,
        });
    };

    /**
     * @dev Updates the `IndexValue` field of an index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     *
     * NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     */
    updateIndexValue = ({
        indexId,
        indexValue,
        userData,
    }: ISuperTokenUpdateIndexValueParams): Operation => {
        return this.idaV1.updateIndexValue({
            indexId,
            indexValue,
            superToken: this.options.address,
            userData,
        });
    };

    /**
     * @dev Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = ({
        indexId,
        subscriber,
        units,
        userData,
    }: ISuperTokenUpdateSubscriptionUnitsParams): Operation => {
        return this.idaV1.updateSubscriptionUnits({
            indexId,
            superToken: this.options.address,
            subscriber,
            units,
            userData,
        });
    };

    /**
     * @dev Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The publisher address whose subscription you want to approve.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = ({
        indexId,
        publisher,
        userData,
    }: ISuperTokenBaseIDAParams): Operation => {
        return this.idaV1.approveSubscription({
            indexId,
            superToken: this.options.address,
            publisher,
            userData,
        });
    };

    /**
     * @dev Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The index publisher address you want to revoke for the subscriber.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = ({
        indexId,
        publisher,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Operation => {
        return this.idaV1.revokeSubscription({
            indexId,
            superToken: this.options.address,
            publisher,
            userData,
        });
    };

    /**
     * @dev Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = ({
        indexId,
        subscriber,
        publisher,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Operation => {
        return this.idaV1.deleteSubscription({
            indexId,
            superToken: this.options.address,
            subscriber,
            publisher,
            userData,
        });
    };

    /**
     * @dev Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = ({
        indexId,
        subscriber,
        publisher,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Operation => {
        return this.idaV1.claim({
            indexId,
            superToken: this.options.address,
            subscriber,
            publisher,
            userData,
        });
    };
}
