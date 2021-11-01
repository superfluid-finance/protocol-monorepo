import { ethers } from "ethers";
import { networkNameToChainIdMap } from "./constants";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { getNetworkName } from "./frameworkHelpers";
import { SuperToken as ISuperToken } from "./typechain";
import {
    ChainId,
    IConfig,
    ISuperTokenBaseIDAParams,
    ISuperTokenBaseSubscriptionParams,
    ISuperTokenCreateFlowParams,
    ISuperTokenDeleteFlowParams,
    ISuperTokenDistributeParams,
    ISuperTokenUpdateFlowParams,
    ISuperTokenUpdateIndexValueParams,
    ISuperTokenUpdateSubscriptionUnitsParams,
    NetworkName,
} from "./interfaces";
import Operation from "./Operation";
import { handleError } from "./errorHelper";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";

export interface ITokenConstructorOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId?: ChainId;
    readonly networkName?: NetworkName;
}
export interface ITokenOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId: ChainId;
    readonly networkName: NetworkName;
}

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
    balanceOf = async (address: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.balanceOf(
                    address
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting balanceOf",
                JSON.stringify(err)
            );
        }
    };

    realtimeBalanceOf = async (address: string, timestamp: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.realtimeBalanceOf(
                    address,
                    timestamp
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOf",
                JSON.stringify(err)
            );
        }
    };

    realtimeBalanceOfNow = async (address: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.realtimeBalanceOfNow(
                    address
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOfNow",
                JSON.stringify(err)
            );
        }
    };

    // SuperToken Contract Write Functions
    approve = async (recipient: string, amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.approve(
                    recipient,
                    amount
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

    downgrade = async (amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.downgrade(
                    amount
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

    transfer = async (recipient: string, amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.transfer(
                    recipient,
                    amount
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

    transferFrom = async (
        sender: string,
        recipient: string,
        amount: string
    ) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.transferFrom(
                    sender,
                    recipient,
                    amount
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

    upgrade = async (amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.upgrade(
                    amount
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

    // CFA Functions
    /**
     * Create a flow of the token of this class.
     * @param
     * @returns {Promise<Operation>}
     */
    createFlow = async ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ISuperTokenCreateFlowParams): Promise<Operation> => {
        return await this.cfaV1.createFlow({
            flowRate,
            receiver,
            sender,
            token: this.options.address,
            userData,
        });
    };

    /**
     * Update a flow of the token of this class.
     * @param
     * @returns {Promise<Operation>}
     */
    updateFlow = async ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ISuperTokenUpdateFlowParams): Promise<Operation> => {
        return await this.cfaV1.updateFlow({
            flowRate,
            receiver,
            sender,
            token: this.options.address,
            userData,
        });
    };

    /**
     * Delete a flow of the token of this class.
     * @param
     * @returns {Promise<Operation>}
     */
    deleteFlow = async ({
        sender,
        receiver,
        userData,
    }: ISuperTokenDeleteFlowParams): Promise<Operation> => {
        return await this.cfaV1.deleteFlow({
            token: this.options.address,
            sender,
            receiver,
            userData,
        });
    };

    // IDA Functions
    createIndex = async ({
        indexId,
        userData,
    }: ISuperTokenBaseIDAParams): Promise<Operation> => {
        return await this.idaV1.createIndex({
            indexId,
            superToken: this.options.address,
            userData,
        });
    };

    distribute = async ({
        indexId,
        amount,
        userData,
    }: ISuperTokenDistributeParams): Promise<Operation> => {
        return await this.idaV1.distribute({
            indexId,
            amount,
            superToken: this.options.address,
            userData,
        });
    };

    updateIndexValue = async ({
        indexId,
        indexValue,
        userData,
    }: ISuperTokenUpdateIndexValueParams): Promise<Operation> => {
        return await this.idaV1.updateIndexValue({
            indexId,
            indexValue,
            superToken: this.options.address,
            userData,
        });
    };

    updateSubscriptionUnits = async ({
        indexId,
        subscriber,
        units,
        userData,
    }: ISuperTokenUpdateSubscriptionUnitsParams): Promise<Operation> => {
        return await this.idaV1.updateSubscriptionUnits({
            indexId,
            superToken: this.options.address,
            subscriber,
            units,
            userData,
        });
    };

    approveSubscription = async ({
        indexId,
        subscriber,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Promise<Operation> => {
        return await this.idaV1.approveSubscription({
            indexId,
            superToken: this.options.address,
            subscriber,
            userData,
        });
    };

    revokeSubscription = async ({
        indexId,
        subscriber,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Promise<Operation> => {
        return await this.idaV1.revokeSubscription({
            indexId,
            superToken: this.options.address,
            subscriber,
            userData,
        });
    };

    deleteSubscription = async ({
        indexId,
        subscriber,
        publisher,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Promise<Operation> => {
        return await this.idaV1.deleteSubscription({
            indexId,
            superToken: this.options.address,
            subscriber,
            publisher,
            userData,
        });
    };

    claim = async ({
        indexId,
        subscriber,
        publisher,
        userData,
    }: ISuperTokenBaseSubscriptionParams): Promise<Operation> => {
        return await this.idaV1.claim({
            indexId,
            superToken: this.options.address,
            subscriber,
            publisher,
            userData,
        });
    };
}
