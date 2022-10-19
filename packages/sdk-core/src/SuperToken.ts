import {
    ISETH,
    ISETH__factory,
    ISuperToken,
    ISuperToken__factory,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { BytesLike, ethers, Overrides } from "ethers";

import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import ERC20Token from "./ERC20Token";
import Governance from "./Governance";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";
import Operation from "./Operation";
import { SFError } from "./SFError";
import { chainIdToResolverDataMap, networkNameToChainIdMap } from "./constants";
import { getNetworkName } from "./frameworkHelpers";
import {
    IConfig,
    IRealtimeBalanceOfParams,
    ISuperTokenBaseIDAParams,
    ISuperTokenCreateFlowByOperatorParams,
    ISuperTokenCreateFlowParams,
    ISuperTokenDeleteFlowParams,
    ISuperTokenDistributeParams,
    ISuperTokenFlowOperatorDataByIDParams,
    ISuperTokenFlowOperatorDataParams,
    ISuperTokenFullControlParams,
    ISuperTokenGetFlowInfoParams,
    ISuperTokenGetFlowParams,
    ISuperTokenGetIndexParams,
    ISuperTokenGetSubscriptionParams,
    ISuperTokenPublisherOperationParams,
    ISuperTokenPubSubParams,
    ISuperTokenUpdateFlowByOperatorParams,
    ISuperTokenUpdateFlowOperatorPermissionsParams,
    ISuperTokenUpdateFlowParams,
    ISuperTokenUpdateIndexValueParams,
    ISuperTokenUpdateSubscriptionUnitsParams,
    IWeb3FlowInfo,
    IWeb3FlowOperatorData,
    IWeb3GovernanceParams,
    IWeb3Index,
    IWeb3RealTimeBalanceOf,
    IWeb3Subscription,
} from "./interfaces";
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
}

export interface ITokenOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly provider: ethers.providers.Provider;
    readonly chainId?: number;
    readonly networkName?: string;
}

/**
 * SuperToken Helper Class
 * @description A helper class to create `SuperToken` objects which can interact with the `SuperToken` contract as well as the CFAV1 and IDAV1 contracts of the desired `SuperToken`.
 * @see https://www.notion.so/superfluidhq/Classification-of-Super-Tokens-5beace780b5c4d09a5752a3677da3dc0 for further details on naming classification and underlying implementation.
 */
export default abstract class SuperToken extends ERC20Token {
    readonly options: ITokenOptions;
    readonly settings: ITokenSettings;
    readonly cfaV1: ConstantFlowAgreementV1;
    readonly idaV1: InstantDistributionAgreementV1;
    readonly governance: Governance;
    readonly underlyingToken?: ERC20Token;
    override readonly contract: ISuperToken;

    protected constructor(options: ITokenOptions, settings: ITokenSettings) {
        // initialize ERC20 token functions here
        super(settings.address);

        this.options = options;
        this.settings = settings;
        this.cfaV1 = new ConstantFlowAgreementV1(
            settings.config.hostAddress,
            settings.config.cfaV1Address,
            settings.config.cfaV1ForwarderAddress
        );
        this.idaV1 = new InstantDistributionAgreementV1(
            settings.config.hostAddress,
            settings.config.idaV1Address
        );
        this.governance = new Governance(
            settings.config.hostAddress,
            settings.config.governanceAddress
        );

        this.contract = new ethers.Contract(
            settings.address,
            ISuperToken__factory.abi
        ) as ISuperToken;
    }

    static create = async (options: ITokenOptions): Promise<SuperToken> => {
        if (!options.chainId && !options.networkName) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                message: "You must input chainId or networkName.",
            });
        }
        const networkName = getNetworkName(options);
        const chainId =
            options.chainId || networkNameToChainIdMap.get(networkName)!;
        try {
            const superToken = ISuperToken__factory.connect(
                options.address,
                options.provider
            );
            const underlyingTokenAddress = await superToken
                .connect(options.provider)
                .getUnderlyingToken();
            const settings: ITokenSettings = {
                address: options.address,
                config: options.config,
                chainId,
                networkName,
            };

            const tokenSymbol = await superToken
                .connect(options.provider)
                .symbol();
            const resolverData = chainIdToResolverDataMap.get(chainId) || {
                subgraphAPIEndpoint: "",
                resolverAddress: "",
                networkName: "",
                nativeTokenSymbol: "",
            };
            const nativeTokenSymbol = resolverData.nativeTokenSymbol || "ETH";
            const nativeSuperTokenSymbol = nativeTokenSymbol + "x";

            if (nativeSuperTokenSymbol === tokenSymbol) {
                return new NativeAssetSuperToken(
                    options,
                    settings,
                    nativeTokenSymbol
                );
            }

            if (underlyingTokenAddress !== ethers.constants.AddressZero) {
                return new WrapperSuperToken(options, {
                    ...settings,
                    underlyingTokenAddress,
                });
            }
            return new PureSuperToken(options, settings);
        } catch (err) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                message: "There was an error initializing the SuperToken",
                cause: err,
            });
        }
    };

    /** ### SuperToken Contract Read Functions ### */

    /**
     * Returns the real time balance of `address`.
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
            const realtimeBalanceOf = await this.contract
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
                message: "There was an error getting realtimeBalanceOf",
                cause: err,
            });
        }
    };

    /** ### CFA Read Functions ### */

    /**
     * Get the details of a flow.
     * @param sender the sender of the flow
     * @param receiver the receiver of the flow
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getFlow = async (
        params: ISuperTokenGetFlowParams
    ): Promise<IWeb3FlowInfo> => {
        return await this.cfaV1.getFlow({
            superToken: this.settings.address,
            sender: params.sender,
            receiver: params.receiver,
            providerOrSigner: params.providerOrSigner,
        });
    };

    /**
     * Get the flow info of an account (net flow).
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getAccountFlowInfo = async (
        params: ISuperTokenGetFlowInfoParams
    ): Promise<IWeb3FlowInfo> => {
        return await this.cfaV1.getAccountFlowInfo({
            superToken: this.settings.address,
            account: params.account,
            providerOrSigner: params.providerOrSigner,
        });
    };

    /**
     * Get the net flow of an account.
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<string>} Web3 Flow info object
     */
    getNetFlow = async (
        params: ISuperTokenGetFlowInfoParams
    ): Promise<string> => {
        return await this.cfaV1.getNetFlow({
            superToken: this.settings.address,
            account: params.account,
            providerOrSigner: params.providerOrSigner,
        });
    };

    /**
     * Get flow operator data.
     * @param sender the sender
     * @param flowOperator the flowOperator
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowOperatorData>} Web3 Flow info object
     */
    getFlowOperatorData = async (
        params: ISuperTokenFlowOperatorDataParams
    ): Promise<IWeb3FlowOperatorData> => {
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        return await this.cfaV1.getFlowOperatorData({
            superToken: this.settings.address,
            sender: normalizedSender,
            flowOperator: normalizedFlowOperator,
            providerOrSigner: params.providerOrSigner,
        });
    };

    /**
     * Get flow operator data using the flowOperatorId.
     * @param flowOperatorId The keccak256 hash of encoded string "flowOperator", sender and flowOperator
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowOperatorData>} Web3 Flow info object
     */
    getFlowOperatorDataByID = async (
        params: ISuperTokenFlowOperatorDataByIDParams
    ): Promise<IWeb3FlowOperatorData> => {
        return await this.cfaV1.getFlowOperatorDataByID({
            superToken: this.settings.address,
            flowOperatorId: params.flowOperatorId,
            providerOrSigner: params.providerOrSigner,
        });
    };

    /** ### CFA Write Functions ### */

    /**
     * Create a flow of the token of this class.
     * @param receiver The receiver of the flow.
     * @param flowRate The specified flow rate.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = (params: ISuperTokenCreateFlowParams): Operation => {
        return this.cfaV1.createFlow({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Update a flow of the token of this class.
     * @param receiver The receiver of the flow.
     * @param flowRate The specified flow rate.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = (params: ISuperTokenUpdateFlowParams): Operation => {
        return this.cfaV1.updateFlow({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Delete a flow of the token of this class.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = (params: ISuperTokenDeleteFlowParams): Operation => {
        return this.cfaV1.deleteFlow({
            superToken: this.settings.address,
            ...params,
        });
    };

    /** ### CFA ACL Write Functions (byOperator) ### */

    /**
     * Update permissions for a flow operator as a sender.
     * @param flowOperator The permission grantee address
     * @param permission The permissions to set.
     * @param flowRateAllowance The flowRateAllowance granted to the flow operator.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlowOperatorPermissions(
        params: ISuperTokenUpdateFlowOperatorPermissionsParams
    ): Operation {
        return this.cfaV1.updateFlowOperatorPermissions({
            superToken: this.settings.address,
            ...params,
        });
    }

    /**
     * Give flow operator full control - max flow rate and create/update/delete permissions.
     * @param flowOperator The permission grantee address
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     */
    authorizeFlowOperatorWithFullControl(
        params: ISuperTokenFullControlParams
    ): Operation {
        return this.cfaV1.authorizeFlowOperatorWithFullControl({
            superToken: this.settings.address,
            ...params,
        });
    }

    /**
     * Revoke flow operator control - set flow rate to 0 with no permissions.
     * @param flowOperator The permission grantee address
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     */
    revokeFlowOperatorWithFullControl(
        params: ISuperTokenFullControlParams
    ): Operation {
        return this.cfaV1.revokeFlowOperatorWithFullControl({
            superToken: this.settings.address,
            ...params,
        });
    }

    /**
     * Create a flow as an operator
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlowByOperator = (
        params: ISuperTokenCreateFlowByOperatorParams
    ): Operation => {
        return this.cfaV1.createFlowByOperator({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Update a flow as an operator.
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlowByOperator = (
        params: ISuperTokenUpdateFlowByOperatorParams
    ): Operation => {
        return this.cfaV1.updateFlowByOperator({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Delete a flow as an operator.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlowByOperator = (params: ISuperTokenDeleteFlowParams): Operation => {
        return this.cfaV1.deleteFlowByOperator({
            superToken: this.settings.address,
            ...params,
        });
    };

    /** ### IDA Read Functions ### */

    /**
     * Get the details of a `Subscription`.
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param subscriber the subscriber's address
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Subscription>} Web3 Subscription object
     */
    getSubscription = async (
        params: ISuperTokenGetSubscriptionParams
    ): Promise<IWeb3Subscription> => {
        return await this.idaV1.getSubscription({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Get the details of an `Index`.
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Index>} Web3 Index object
     */
    getIndex = async (
        params: ISuperTokenGetIndexParams
    ): Promise<IWeb3Index> => {
        return await this.idaV1.getIndex({
            superToken: this.settings.address,
            ...params,
        });
    };

    /** ### IDA Write Functions ### */

    /**
     * Creates an IDA Index.
     * @param indexId The id of the index.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = (params: ISuperTokenBaseIDAParams): Operation => {
        return this.idaV1.createIndex({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Distributes `amount` of token to an index
     * @param indexId The id of the index.
     * @param amount The amount of tokens to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = (params: ISuperTokenDistributeParams): Operation => {
        return this.idaV1.distribute({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Updates the `IndexValue` field of an index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     *
     * NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     */
    updateIndexValue = (
        params: ISuperTokenUpdateIndexValueParams
    ): Operation => {
        return this.idaV1.updateIndexValue({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = (
        params: ISuperTokenUpdateSubscriptionUnitsParams
    ): Operation => {
        return this.idaV1.updateSubscriptionUnits({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The publisher address whose subscription you want to approve.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = (
        params: ISuperTokenPublisherOperationParams
    ): Operation => {
        return this.idaV1.approveSubscription({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param publisher The index publisher address you want to revoke for the subscriber.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = (
        params: ISuperTokenPublisherOperationParams
    ): Operation => {
        return this.idaV1.revokeSubscription({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = (params: ISuperTokenPubSubParams): Operation => {
        return this.idaV1.deleteSubscription({
            superToken: this.settings.address,
            ...params,
        });
    };

    /**
     * Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param subscriber The subscriber address who you are claiming for.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = (params: ISuperTokenPubSubParams): Operation => {
        return this.idaV1.claim({
            superToken: this.settings.address,
            ...params,
        });
    };

    /** ### Governance Read Functions ### */

    getGovernanceParameters = async (
        providerOrSigner: ethers.providers.Provider | ethers.Signer
    ): Promise<IWeb3GovernanceParams> => {
        return this.governance.getGovernanceParameters({
            providerOrSigner,
            token: this.settings.address,
        });
    };
}

/**
 * WrapperSuperToken has an underlying ERC20 token.
 */
export class WrapperSuperToken extends SuperToken {
    override readonly underlyingToken: ERC20Token;

    constructor(
        options: ITokenOptions,
        settings: ITokenSettings & { underlyingTokenAddress: string }
    ) {
        super(options, settings);
        this.underlyingToken = new ERC20Token(settings.underlyingTokenAddress);
    }

    /** ### WrapperSuperToken Contract Write Functions ### */

    /**
     * Downgrade `amount` SuperToken's.
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
        const txn = this.contract.populateTransaction.downgrade(
            amount,
            overrides || {}
        );
        return new Operation(txn, "SUPERTOKEN_DOWNGRADE");
    };

    /**
     * Upgrade `amount` SuperToken's.
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
        const txn = this.contract.populateTransaction.upgrade(
            amount,
            overrides || {}
        );
        return new Operation(txn, "SUPERTOKEN_UPGRADE");
    };

    /**
     * Upgrade `amount` of an ERC20 token to its SuperToken to `to` address.
     * @param amount The amount to be upgraded.
     * @param to The destination of the upgraded native asset super tokens.
     * @param data Bytes operatorData
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    upgradeTo = ({
        amount,
        to,
        data = "0x",
        overrides,
    }: {
        amount: string;
        to: string;
        data?: BytesLike;
        overrides?: Overrides & { from?: string | Promise<string> };
    }) => {
        const txn = this.contract.populateTransaction.upgradeTo(
            to,
            amount,
            data,
            {
                ...overrides,
            }
        );
        return new Operation(txn, "UNSUPPORTED");
    };
}

/**
 * PureSuperToken doesn't have any underlying ERC20 token.
 */
export class PureSuperToken extends SuperToken {
    constructor(options: ITokenOptions, settings: ITokenSettings) {
        super(options, settings);
    }
}

/**
 * NativeAssetSuperToken wraps the native asset of the network.
 */
export class NativeAssetSuperToken extends SuperToken {
    readonly nativeTokenSymbol: string;
    constructor(
        options: ITokenOptions,
        settings: ITokenSettings,
        nativeTokenSymbol: string
    ) {
        super(options, settings);
        this.nativeTokenSymbol = nativeTokenSymbol;
    }

    get nativeAssetContract() {
        return new ethers.Contract(
            this.settings.address,
            ISETH__factory.abi
        ) as ISETH;
    }

    /**
     * Upgrade `amount` of a network's native asset to its SuperToken.
     * @param amount The amount to be upgraded.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    upgrade = ({
        amount,
        overrides,
    }: {
        amount: string;
        overrides?: Overrides & { from?: string | Promise<string> };
    }): Operation => {
        const txn = this.nativeAssetContract.populateTransaction.upgradeByETH({
            ...overrides,
            value: amount,
        });
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Upgrade `amount` of a network's native asset to its SuperToken to `to` address.
     * @param amount The amount to be upgraded.
     * @param to The destination of the upgraded native asset super tokens.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    upgradeTo = ({
        amount,
        to,
        overrides,
    }: {
        amount: string;
        to: string;
        overrides?: Overrides & { from?: string | Promise<string> };
    }) => {
        const txn = this.nativeAssetContract.populateTransaction.upgradeByETHTo(
            to,
            {
                ...overrides,
                value: amount,
            }
        );
        return new Operation(txn, "UNSUPPORTED");
    };

    /**
     * Downgrade `amount` of a native asset super token to the underlying native asset.
     * @param amount The amount to be upgraded.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed.
     */
    downgrade = ({
        amount,
        overrides,
    }: {
        amount: string;
        overrides?: Overrides & { from?: string | Promise<string> };
    }) => {
        const txn = this.nativeAssetContract.populateTransaction.downgradeToETH(
            amount,
            {
                ...overrides,
            }
        );
        return new Operation(txn, "UNSUPPORTED");
    };
}
