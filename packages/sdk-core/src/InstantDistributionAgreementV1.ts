import { ethers } from "ethers";

import Host from "./Host";
import Operation from "./Operation";
import { SFError } from "./SFError";
import IInstantDistributionAgreementV1ABI from "./abi/IInstantDistributionAgreementV1.json";
import {
    IAgreementV1Options,
    IApproveSubscriptionParams,
    IClaimParams,
    ICreateIndexParams,
    IDeleteSubscriptionParams,
    IDistributeParams,
    IGetIndexParams,
    IGetSubscriptionParams,
    IRevokeSubscriptionParams,
    IUpdateIndexValueParams,
    IUpdateSubscriptionUnitsParams,
    IWeb3Index,
    IWeb3Subscription,
} from "./interfaces";
import { IInstantDistributionAgreementV1 } from "./typechain";
import { normalizeAddress } from "./utils";

const idaInterface = new ethers.utils.Interface(
    IInstantDistributionAgreementV1ABI.abi
);

/**
 * @dev Instant Distribution Agreement V1 Helper Class
 * @description A helper class to interact with the IDAV1 contract.
 */
export default class InstantDistributionAgreementV1 {
    readonly options: IAgreementV1Options;
    readonly host: Host;

    constructor(options: IAgreementV1Options) {
        this.options = options;
        this.host = new Host(options.config.hostAddress);
    }

    private get idaContract() {
        return new ethers.Contract(
            this.options.config.idaV1Address,
            IInstantDistributionAgreementV1ABI.abi
        ) as IInstantDistributionAgreementV1;
    }

    // IDA Read Functions

    /**
     * @dev Get the details of a `Subscription`.
     * @param superToken the superToken of the agreement
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param subscriber the subscriber's address
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Subscription>} Web3 Subscription object
     */
    getSubscription = async ({
        superToken,
        publisher,
        indexId,
        subscriber,
        providerOrSigner,
    }: IGetSubscriptionParams): Promise<IWeb3Subscription> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedSubscriber = normalizeAddress(subscriber);
        try {
            const subscription = await this.idaContract
                .connect(providerOrSigner)
                .getSubscription(
                    normalizedToken,
                    normalizedPublisher,
                    indexId,
                    normalizedSubscriber
                );

            return {
                exist: subscription.exist,
                approved: subscription.approved,
                units: subscription.units.toString(),
                pendingDistribution:
                    subscription.pendingDistribution.toString(),
            };
        } catch (err) {
            throw new SFError({
                type: "IDAV1_READ",
                customMessage: "There was an error getting the subscription",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Get the details of an `Index`.
     * @param superToken the superToken of the agreement
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Index>} Web3 Index object
     */
    getIndex = async ({
        superToken,
        publisher,
        indexId,
        providerOrSigner,
    }: IGetIndexParams): Promise<IWeb3Index> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedPublisher = normalizeAddress(publisher);
        try {
            const index = await this.idaContract
                .connect(providerOrSigner)
                .getIndex(normalizedToken, normalizedPublisher, indexId);
            return {
                exist: index.exist,
                indexValue: index.indexValue.toString(),
                totalUnitsApproved: index.totalUnitsApproved.toString(),
                totalUnitsPending: index.totalUnitsPending.toString(),
            };
        } catch (err) {
            throw new SFError({
                type: "IDAV1_READ",
                customMessage: "There was an error getting the index",
                errorObject: err,
            });
        }
    };

    // IDA Write Functions

    /**
     * @dev Creates an IDA Index.
     * @param indexId The id of the index.
     * @param superToken The address of the `index` superToken.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = ({
        indexId,
        superToken,
        userData,
        overrides,
    }: ICreateIndexParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("createIndex", [
            normalizedToken,
            indexId,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Distributes `amount` of `superToken` to an index
     * @param indexId The id of the index.
     * @param amount The amount of `superToken` to be distributed.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = ({
        indexId,
        amount,
        superToken,
        userData,
        overrides,
    }: IDistributeParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("distribute", [
            normalizedToken,
            indexId,
            amount,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Updates the `indexValue` of an index.
     * @description NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateIndexValue = ({
        indexId,
        indexValue,
        superToken,
        userData,
        overrides,
    }: IUpdateIndexValueParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("updateIndex", [
            normalizedToken,
            indexId,
            indexValue,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = ({
        indexId,
        superToken,
        subscriber,
        units,
        userData,
        overrides,
    }: IUpdateSubscriptionUnitsParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSubscriber = normalizeAddress(subscriber);
        const callData = idaInterface.encodeFunctionData("updateSubscription", [
            normalizedToken,
            indexId,
            normalizedSubscriber,
            units,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param publisher The publisher of the index you want to approve.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = ({
        indexId,
        superToken,
        publisher,
        userData,
        overrides,
    }: IApproveSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData(
            "approveSubscription",
            [normalizedToken, normalizedPublisher, indexId, "0x"]
        );

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributres.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to revoke.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = ({
        indexId,
        superToken,
        publisher,
        userData,
        overrides,
    }: IRevokeSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("revokeSubscription", [
            normalizedToken,
            normalizedPublisher,
            indexId,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
        overrides,
    }: IDeleteSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSubscriber = normalizeAddress(subscriber);
        const callData = idaInterface.encodeFunctionData("deleteSubscription", [
            normalizedToken,
            normalizedPublisher,
            indexId,
            normalizedSubscriber,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
        overrides,
    }: IClaimParams): Operation => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSubscriber = normalizeAddress(subscriber);
        const callData = idaInterface.encodeFunctionData("claim", [
            normalizedToken,
            normalizedPublisher,
            indexId,
            normalizedSubscriber,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData,
            overrides
        );
    };
}
