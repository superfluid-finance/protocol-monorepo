import { ethers } from "ethers";
import { abi as IInstantDistributionAgreementV1ABI } from "./abi/IInstantDistributionAgreementV1.json";
import {
    IAgreementV1Options,
    IBaseIDAParams,
    IBaseSubscriptionParams,
    IDistributeParams,
    IUpdateIndexValueParams,
    IUpdateSubscriptionUnitsParams,
} from "./interfaces";
import { normalizeAddress } from "./utils";
import Operation from "./Operation";
import Host from "./Host";

const idaInterface = new ethers.utils.Interface(
    IInstantDistributionAgreementV1ABI
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

    /**
     * @dev Creates an IDA Index.
     * @param indexId The id of the index.
     * @param superToken The address of the `index` superToken.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createIndex = ({
        indexId,
        superToken,
        userData,
    }: IBaseIDAParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("createIndex", [
            normalizedToken,
            indexId,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData
        );
    };

    /**
     * @dev Distributes `amount` of `superToken` to an index
     * @param indexId The id of the index.
     * @param amount The amount of `superToken` to be distributed.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    distribute = ({
        indexId,
        amount,
        superToken,
        userData,
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
            userData
        );
    };

    /**
     * @dev Updates the `indexValue` of an index.
     * @description NOTE: It has the same effect as `distribute`, but is closer to the low level data structure of the index.
     * @param indexId The id of the index.
     * @param indexValue The new indexValue.
     * @param superToken The superToken to be distributed.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateIndexValue = ({
        indexId,
        indexValue,
        superToken,
        userData,
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
            userData
        );
    };

    /**
     * @dev Updates the `units` allocated to a Subscription.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose units you want to update.
     * @param units The amount of units you want to update to.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateSubscriptionUnits = ({
        indexId,
        superToken,
        subscriber,
        units,
        userData,
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
            userData
        );
    };

    /**
     * @dev Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param publisher The publisher of the index you want to approve.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    approveSubscription = ({
        indexId,
        superToken,
        publisher,
        userData,
    }: IBaseIDAParams): Operation => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData(
            "approveSubscription",
            [normalizedToken, normalizedPublisher, indexId, "0x"]
        );

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.idaV1Address,
            callData,
            userData
        );
    };

    /**
     * @dev Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributres.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to revoke.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    revokeSubscription = ({
        indexId,
        superToken,
        publisher,
        userData,
    }: IBaseIDAParams): Operation => {
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
            userData
        );
    };

    /**
     * @dev Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteSubscription = ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
    }: IBaseSubscriptionParams): Operation => {
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
            userData
        );
    };

    /**
     * @dev Claims any pending tokens allocated to the Subscription (unapproved).
     * @param indexId The id of the index.
     * @param superToken The superToken of the index.
     * @param subscriber The subscriber address whose subscription you want to delete.
     * @param publisher The publisher address of the index you are targetting.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    claim = ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
    }: IBaseSubscriptionParams): Operation => {
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
            userData
        );
    };
}
