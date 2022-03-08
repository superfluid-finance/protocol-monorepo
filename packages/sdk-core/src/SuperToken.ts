import { ethers, Overrides } from "ethers";

import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";
import Operation from "./Operation";
import { SFError } from "./SFError";
import Token from "./Token";
import SuperTokenABI from "./abi/SuperToken.json";
import { networkNameToChainIdMap } from "./constants";
import { getNetworkName } from "./frameworkHelpers";
import {
    IConfig,
    IRealtimeBalanceOfParams,
    ISuperTokenBaseIDAParams,
    ISuperTokenCreateFlowParams,
    ISuperTokenDeleteFlowParams,
    ISuperTokenDistributeParams,
    ISuperTokenGetFlowInfoParams,
    ISuperTokenGetFlowParams,
    ISuperTokenGetIndexParams,
    ISuperTokenGetSubscriptionParams,
    ISuperTokenPublisherOperationParams,
    ISuperTokenPubSubParams,
    ISuperTokenUpdateFlowParams,
    ISuperTokenUpdateIndexValueParams,
    ISuperTokenUpdateSubscriptionUnitsParams,
    IWeb3FlowInfo,
    IWeb3Index,
    IWeb3RealTimeBalanceOf,
    IWeb3Subscription,
} from "./interfaces";
import { SuperToken as ISuperToken } from "./typechain";
import {
    getSanitizedTimestamp,
    getStringCurrentTimeInSeconds,
    normalizeAddress,
} from "./utils";

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
    readonly provider: ethers.providers.Provider;
    readonly chainId?: number;
    readonly networkName?: string;
}

/**
 * @dev SuperToken Helper Class
 * @description A helper class to create `SuperToken` objects which can interact with the `SuperToken` contract as well as the CFAV1 and IDAV1 contracts of the desired `SuperToken`.
 */
export default class SuperToken extends Token {
    readonly options: ITokenOptions;
    readonly settings: ITokenSettings;
    readonly cfaV1: ConstantFlowAgreementV1;
    readonly idaV1: InstantDistributionAgreementV1;
    readonly underlyingToken: Token;

    private constructor(options: ITokenOptions, settings: ITokenSettings) {
        // initialize ERC20 token functions here
        super(settings.address);

        this.options = options;
        this.settings = settings;
        this.cfaV1 = new ConstantFlowAgreementV1({
            config: this.settings.config,
        });
        this.idaV1 = new InstantDistributionAgreementV1({
            config: this.settings.config,
        });
        this.underlyingToken = new Token(settings.underlyingTokenAddress);
    }

    static create = async (options: ITokenOptions) => {
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
                SuperTokenABI.abi
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
            return new SuperToken(options, settings);
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
            this.settings.address,
            SuperTokenABI.abi
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
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    downgrade = ({
        amount,
        overrides,
    }: {
        amount: string;
        overrides?: Overrides & { from?: string | Promise<string> };
    }): Operation => {
        const txn = this.superTokenContract.populateTransaction.downgrade(
            amount,
            overrides || {}
        );
        return new Operation(txn, "SUPERTOKEN_DOWNGRADE");
    };

    /**
     * @dev Upgrade `amount` SuperToken's.
     * @param amount The amount to be upgraded.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    upgrade = ({
        amount,
        overrides,
    }: {
        amount: string;
        overrides?: Overrides & { from?: string | Promise<string> };
    }): Operation => {
        const txn = this.superTokenContract.populateTransaction.upgrade(
            amount,
            overrides || {}
        );
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
            superToken: this.settings.address,
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
            superToken: this.settings.address,
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
            superToken: this.settings.address,
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
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = ({
        sender,
        receiver,
        flowRate,
        userData,
        overrides,
    }: ISuperTokenCreateFlowParams): Operation => {
        return this.cfaV1.createFlow({
            flowRate,
            receiver,
            sender,
            superToken: this.settings.address,
            userData,
            overrides,
        });
    };

    /**
     * @dev Update a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param flowRate The specified flow rate.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = ({
        sender,
        receiver,
        flowRate,
        userData,
        overrides,
    }: ISuperTokenUpdateFlowParams): Operation => {
        return this.cfaV1.updateFlow({
            flowRate,
            receiver,
            sender,
            superToken: this.settings.address,
            userData,
            overrides,
        });
    };

    /**
     * @dev Delete a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = ({
        sender,
        receiver,
        userData,
        overrides,
    }: ISuperTokenDeleteFlowParams): Operation => {
        return this.cfaV1.deleteFlow({
            superToken: this.settings.address,
            sender,
            receiver,
            userData,
            overrides,
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
            superToken: this.settings.address,
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
            superToken: this.settings.address,
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
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = ({
        indexId,
        userData,
        overrides,
    }: ISuperTokenBaseIDAParams): Operation => {
        return this.idaV1.createIndex({
            indexId,
            superToken: this.settings.address,
            userData,
            overrides,
        });
    };

    /**
     * @dev Distributes `amount` of token to an index
     * @param indexId The id of the index.
     * @param amount The amount of tokens to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = ({
        indexId,
        amount,
        userData,
        overrides,
    }: ISuperTokenDistributeParams): Operation => {
        return this.idaV1.distribute({
            indexId,
            amount,
            superToken: this.settings.address,
            userData,
            overrides,
        });
    };

    /**
     * @dev Updates the `IndexValue` field of an index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     *
     * NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     */
    updateIndexValue = ({
        indexId,
        indexValue,
        userData,
        overrides,
    }: ISuperTokenUpdateIndexValueParams): Operation => {
        return this.idaV1.updateIndexValue({
            indexId,
            indexValue,
            superToken: this.settings.address,
            userData,
            overrides,
        });
    };

    /**
     * @dev Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = ({
        indexId,
        subscriber,
        units,
        userData,
        overrides,
    }: ISuperTokenUpdateSubscriptionUnitsParams): Operation => {
        return this.idaV1.updateSubscriptionUnits({
            indexId,
            superToken: this.settings.address,
            subscriber,
            units,
            userData,
            overrides,
        });
    };

    /**
     * @dev Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The publisher address whose subscription you want to approve.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = ({
        indexId,
        publisher,
        userData,
        overrides,
    }: ISuperTokenPublisherOperationParams): Operation => {
        return this.idaV1.approveSubscription({
            indexId,
            superToken: this.settings.address,
            publisher,
            userData,
            overrides,
        });
    };

    /**
     * @dev Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The index publisher address you want to revoke for the subscriber.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = ({
        indexId,
        publisher,
        userData,
        overrides,
    }: ISuperTokenPublisherOperationParams): Operation => {
        return this.idaV1.revokeSubscription({
            indexId,
            superToken: this.settings.address,
            publisher,
            userData,
            overrides,
        });
    };

    /**
     * @dev Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = ({
        indexId,
        subscriber,
        publisher,
        userData,
        overrides,
    }: ISuperTokenPubSubParams): Operation => {
        return this.idaV1.deleteSubscription({
            indexId,
            superToken: this.settings.address,
            subscriber,
            publisher,
            userData,
            overrides,
        });
    };

    /**
     * @dev Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param subscriber The subscriber address who you are claiming for.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = ({
        indexId,
        subscriber,
        publisher,
        userData,
        overrides,
    }: ISuperTokenPubSubParams): Operation => {
        return this.idaV1.claim({
            indexId,
            superToken: this.settings.address,
            subscriber,
            publisher,
            userData,
            overrides,
        });
    };
}
