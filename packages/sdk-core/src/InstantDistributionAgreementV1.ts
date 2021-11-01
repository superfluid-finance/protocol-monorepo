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
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { Superfluid } from "./typechain";
import { normalizeAddress } from "./utils";
import { handleError } from "./errorHelper";
import Operation from "./Operation";

const idaInterface = new ethers.utils.Interface(
    IInstantDistributionAgreementV1ABI
);

export default class InstantDistributionAgreementV1 {
    readonly options: IAgreementV1Options;

    constructor(options: IAgreementV1Options) {
        this.options = options;
    }

    get hostContract() {
        return new ethers.Contract(
            this.options.config.hostAddress,
            SuperfluidABI
        ) as Superfluid;
    }

    private populateTransactionAndReturnOperation = async (
        callData: string,
        userData: string | undefined
    ) => {
        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    this.options.config.idaV1Address,
                    callData,
                    userData || "0x"
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    createIndex = async ({ indexId, superToken, userData }: IBaseIDAParams) => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("createIndex", [
            normalizedToken,
            indexId,
            "0x",
        ]);

        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    distribute = async ({
        indexId,
        amount,
        superToken,
        userData,
    }: IDistributeParams) => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("distribute", [
            normalizedToken,
            indexId,
            amount,
            "0x",
        ]);
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    updateIndexValue = async ({
        indexId,
        indexValue,
        superToken,
        userData,
    }: IUpdateIndexValueParams) => {
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("updateIndex", [
            normalizedToken,
            indexId,
            indexValue,
            "0x",
        ]);
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    updateSubscriptionUnits = async ({
        indexId,
        superToken,
        subscriber,
        units,
        userData,
    }: IUpdateSubscriptionUnitsParams) => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSubscriber = normalizeAddress(subscriber);
        const callData = idaInterface.encodeFunctionData("updateSubscription", [
            normalizedToken,
            indexId,
            normalizedSubscriber,
            units,
            "0x",
        ]);
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    approveSubscription = async ({
        indexId,
        superToken,
        publisher,
        userData,
    }: IBaseSubscriptionParams) => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData(
            "approveSubscription",
            [normalizedToken, normalizedPublisher, indexId, "0x"]
        );
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    revokeSubscription = async ({
        indexId,
        superToken,
        publisher,
        userData,
    }: IBaseSubscriptionParams) => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const callData = idaInterface.encodeFunctionData("revokeSubscription", [
            normalizedToken,
            normalizedPublisher,
            indexId,
            "0x",
        ]);
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    deleteSubscription = async ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
    }: IBaseSubscriptionParams) => {
        const normalizedPublisher = normalizeAddress(publisher);
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSubscriber = normalizeAddress(subscriber);
        const callData = idaInterface.encodeFunctionData("revokeSubscription", [
            normalizedToken,
            normalizedPublisher,
            indexId,
            normalizedSubscriber,
            "0x",
        ]);
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    claim = async ({
        indexId,
        superToken,
        subscriber,
        publisher,
        userData,
    }: IBaseSubscriptionParams) => {
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
        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };
}
