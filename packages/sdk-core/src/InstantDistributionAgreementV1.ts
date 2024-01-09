import { ethers } from "ethers";

import Host from "./Host";
import Operation from "./Operation";
import { SFError } from "./SFError";
import SuperfluidAgreement from "./SuperfluidAgreement";
import {
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
import {
    IInstantDistributionAgreementV1,
    IInstantDistributionAgreementV1__factory,
} from "./typechain-types";
import { normalizeAddress } from "./utils";

const idaInterface = IInstantDistributionAgreementV1__factory.createInterface();

/**
 * Instant Distribution Agreement V1 Helper Class
 * @description A helper class to interact with the IDAV1 contract.
 */
export default class InstantDistributionAgreementV1 extends SuperfluidAgreement {
    readonly host: Host;
    readonly contract: IInstantDistributionAgreementV1;

    constructor(hostAddress: string, idaV1Address: string) {
        super();
        this.host = new Host(hostAddress);
        this.contract = new ethers.Contract(
            idaV1Address,
            IInstantDistributionAgreementV1__factory.abi
        ) as IInstantDistributionAgreementV1;
    }

    /** ### IDA Read Functions ### */

    /**
     * Get the details of a `Subscription`.
     * @param superToken the superToken of the agreement
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param subscriber the subscriber's address
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Subscription>} Web3 Subscription object
     */
    getSubscription = async (
        params: IGetSubscriptionParams
    ): Promise<IWeb3Subscription> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedPublisher = normalizeAddress(params.publisher);
        const normalizedSubscriber = normalizeAddress(params.subscriber);
        try {
            const subscription = await this.contract
                .connect(params.providerOrSigner)
                .getSubscription(
                    normalizedToken,
                    normalizedPublisher,
                    params.indexId,
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
                message: "There was an error getting the subscription",
                cause: err,
            });
        }
    };

    /**
     * Get the details of an `Index`.
     * @param superToken the superToken of the agreement
     * @param publisher the address of the publisher of the index
     * @param indexId the index id
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3Index>} Web3 Index object
     */
    getIndex = async (params: IGetIndexParams): Promise<IWeb3Index> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedPublisher = normalizeAddress(params.publisher);
        try {
            const index = await this.contract
                .connect(params.providerOrSigner)
                .getIndex(normalizedToken, normalizedPublisher, params.indexId);
            return {
                exist: index.exist,
                indexValue: index.indexValue.toString(),
                totalUnitsApproved: index.totalUnitsApproved.toString(),
                totalUnitsPending: index.totalUnitsPending.toString(),
            };
        } catch (err) {
            throw new SFError({
                type: "IDAV1_READ",
                message: "There was an error getting the index",
                cause: err,
            });
        }
    };

    /** ### IDA Write Functions ### */

    /**
     * Creates an IDA Index.
     * @param indexId The id of the index.
     * @param superToken The address of the `index` superToken.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = (params: ICreateIndexParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const callData = idaInterface.encodeFunctionData("createIndex", [
            normalizedToken,
            params.indexId,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Distributes `amount` of `superToken` to an index
     * @param indexId The id of the index.
     * @param amount The amount of `superToken` to be distributed.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = (params: IDistributeParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const callData = idaInterface.encodeFunctionData("distribute", [
            normalizedToken,
            params.indexId,
            params.amount,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Updates the `indexValue` of an index.
     * @description NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateIndexValue = (params: IUpdateIndexValueParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const callData = idaInterface.encodeFunctionData("updateIndex", [
            normalizedToken,
            params.indexId,
            params.indexValue,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = (
        params: IUpdateSubscriptionUnitsParams
    ): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSubscriber = normalizeAddress(params.subscriber);
        const callData = idaInterface.encodeFunctionData("updateSubscription", [
            normalizedToken,
            params.indexId,
            normalizedSubscriber,
            params.units,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param publisher The publisher of the index you want to approve.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = (params: IApproveSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(params.publisher);
        const normalizedToken = normalizeAddress(params.superToken);
        const callData = idaInterface.encodeFunctionData(
            "approveSubscription",
            [normalizedToken, normalizedPublisher, params.indexId, "0x"]
        );

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to revoke.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = (params: IRevokeSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(params.publisher);
        const normalizedToken = normalizeAddress(params.superToken);
        const callData = idaInterface.encodeFunctionData("revokeSubscription", [
            normalizedToken,
            normalizedPublisher,
            params.indexId,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = (params: IDeleteSubscriptionParams): Operation => {
        const normalizedPublisher = normalizeAddress(params.publisher);
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSubscriber = normalizeAddress(params.subscriber);
        const callData = idaInterface.encodeFunctionData("deleteSubscription", [
            normalizedToken,
            normalizedPublisher,
            params.indexId,
            normalizedSubscriber,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };

    /**
     * Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targeting.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = (params: IClaimParams): Operation => {
        const normalizedPublisher = normalizeAddress(params.publisher);
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSubscriber = normalizeAddress(params.subscriber);
        const callData = idaInterface.encodeFunctionData("claim", [
            normalizedToken,
            normalizedPublisher,
            params.indexId,
            normalizedSubscriber,
            "0x",
        ]);

        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    };
}
