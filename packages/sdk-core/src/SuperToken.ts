import { ethers } from "ethers";
import { networkNameToChainIdMap } from "./constants";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { getNetworkName } from "./frameworkHelpers";
import { SuperToken as ISuperToken } from "./typechain";
import {
    IConfig,
    IRealtimeBalanceOfParams,
    ISuperTokenBaseIDAParams,
    ISuperTokenBaseSubscriptionParams,
    ISuperTokenCreateFlowParams,
    ISuperTokenDeleteFlowParams,
    ISuperTokenDistributeParams,
    ISuperTokenGetFlowInfoParams,
    ISuperTokenGetFlowParams,
    ISuperTokenGetIndexParams,
    ISuperTokenGetSubscriptionParams,
    ISuperTokenUpdateFlowParams,
    ISuperTokenUpdateIndexValueParams,
    ISuperTokenUpdateSubscriptionUnitsParams,
    IWeb3FlowInfo,
    IWeb3Index,
    IWeb3RealTimeBalanceOf,
    IWeb3Subscription,
} from "./interfaces";
import Operation from "./Operation";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";
import SFError from "./SFError";
import {
    getSanitizedTimestamp,
    getStringCurrentTimeInSeconds,
    normalizeAddress,
} from "./utils";
import Token from "./Token";

export interface ITokenConstructorOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly provider: ethers.providers.Provider;
    readonly chainId?: number;
    readonly networkName?: string;
}

export interface ITokenSettings {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId: number;
    readonly networkName: string;
    readonly underlyingTokenAddress: string;
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
export default class SuperToken extends Token {
    readonly options: ITokenOptions;
    readonly cfaV1: ConstantFlowAgreementV1;
    readonly idaV1: InstantDistributionAgreementV1;
    readonly underlyingToken: Token;

    private constructor(settings: ITokenSettings) {
        // initialize ERC20 token functions here
        super(settings.address);

        this.options = {
            address: settings.address,
            chainId: settings.chainId,
            networkName: settings.networkName,
            config: settings.config,
        };
        this.cfaV1 = new ConstantFlowAgreementV1({
            config: this.options.config,
        });
        this.idaV1 = new InstantDistributionAgreementV1({
            config: this.options.config,
        });
        this.underlyingToken = new Token(ethers.constants.AddressZero);
    }

    static create = async (options: ITokenConstructorOptions) => {
        if (!options.chainId && !options.networkName) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                customMessage: "You must input chainId or networkName.",
            });
        }
        const networkName = getNetworkName(options);
        const chainId =
            options.chainId || networkNameToChainIdMap.get(networkName)!;
        try {
            const superToken = new ethers.Contract(
                options.address,
                SuperTokenABI
            ) as ISuperToken;
            const underlyingTokenAddress = await superToken
                .connect(options.provider)
                .getUnderlyingToken();
            const settings: ITokenSettings = {
                address: options.address,
                config: options.config,
                chainId,
                networkName,
                underlyingTokenAddress,
            };
            return new SuperToken(settings);
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                customMessage: "There was an error initializing the SuperToken",
                errorObject: err,
            });
        }
    };

    private get superTokenContract() {
        return new ethers.Contract(
            this.options.address,
            SuperTokenABI
        ) as ISuperToken;
    }

    // SuperToken Contract Read Functions

    /**
     * @dev Returns the real time balance of `address`.
     * @param account the target address
     * @param timestamp the timestamp you'd like to see the data
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @returns {Promise<IWeb3RealTimeBalanceOf>} real time balance of data
     */
    realtimeBalanceOf = async ({
        providerOrSigner,
        account,
        timestamp = getStringCurrentTimeInSeconds(),
    }: IRealtimeBalanceOfParams): Promise<IWeb3RealTimeBalanceOf> => {
        const normalizedAccount = normalizeAddress(account);
        try {
            const realtimeBalanceOf = await this.superTokenContract
                .connect(providerOrSigner)
                .realtimeBalanceOf(normalizedAccount, timestamp);
            return {
                availableBalance: realtimeBalanceOf.availableBalance.toString(),
                deposit: realtimeBalanceOf.deposit.toString(),
                owedDeposit: realtimeBalanceOf.owedDeposit.toString(),
                timestamp: getSanitizedTimestamp(timestamp),
            };
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_READ",
                customMessage: "There was an error getting realtimeBalanceOf",
                errorObject: err,
            });
        }
    };

    // SuperToken Contract Write Functions

    /**
     * @dev Downgrade `amount` SuperToken's.
     * @param amount The amount to be downgraded.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    downgrade = ({ amount }: { amount: string }): Operation => {
        const txn =
            this.superTokenContract.populateTransaction.downgrade(amount);
        return new Operation(txn, "SUPERTOKEN_DOWNGRADE");
    };

    /**
     * @dev Upgrade `amount` SuperToken's.
     * @param amount The amount to be upgraded.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    upgrade = ({ amount }: { amount: string }): Operation => {
        const txn = this.superTokenContract.populateTransaction.upgrade(amount);
        return new Operation(txn, "SUPERTOKEN_UPGRADE");
    };

    // CFA Read Functions

    /**
     * @dev Get the details of a flow.
     * @param sender the sender of the flow
     * @param receiver the receiver of the flow
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getFlow = async ({
        sender,
        receiver,
        providerOrSigner,
    }: ISuperTokenGetFlowParams): Promise<IWeb3FlowInfo> => {
        return await this.cfaV1.getFlow({
            superToken: this.options.address,
            sender,
            receiver,
            providerOrSigner,
        });
    };

    /**
     * @dev Get the flow info of an account (net flow).
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getAccountFlowInfo = async ({
        account,
        providerOrSigner,
    }: ISuperTokenGetFlowInfoParams): Promise<IWeb3FlowInfo> => {
        return await this.cfaV1.getAccountFlowInfo({
            superToken: this.options.address,
            account,
            providerOrSigner,
        });
    };

    /**
     * @dev Get the net flow of an account.
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<string>} Web3 Flow info object
     */
    getNetFlow = async ({
        account,
        providerOrSigner,
    }: ISuperTokenGetFlowInfoParams): Promise<string> => {
        return await this.cfaV1.getNetFlow({
            superToken: this.options.address,
            account,
            providerOrSigner,
        });
    };

    // CFA Write Functions

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

    // IDA Read Functions

    /**
     * @dev Get the details of a `Subscription`.
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param subscriber the subscriber's address
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Subscription>} Web3 Subscription object
     */
    getSubscription = async ({
        publisher,
        indexId,
        subscriber,
        providerOrSigner,
    }: ISuperTokenGetSubscriptionParams): Promise<IWeb3Subscription> => {
        return await this.idaV1.getSubscription({
            superToken: this.options.address,
            publisher,
            indexId,
            subscriber,
            providerOrSigner,
        });
    };

    /**
     * @dev Get the details of an `Index`.
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Index>} Web3 Index object
     */
    getIndex = async ({
        publisher,
        indexId,
        providerOrSigner,
    }: ISuperTokenGetIndexParams): Promise<IWeb3Index> => {
        return await this.idaV1.getIndex({
            superToken: this.options.address,
            publisher,
            indexId,
            providerOrSigner,
        });
    };

    // IDA Write Functions

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
    }: ISuperTokenBaseIDAParams): Operation => {
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
