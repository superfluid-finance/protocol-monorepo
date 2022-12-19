import { ethers } from "hardhat";
import { Framework, SuperToken, TestToken } from "@superfluid-finance/sdk-core";
import {
    asleep,
    beforeSetup,
    fetchEntityAndEnsureExistence,
    getATSId,
    getRandomFlowRate,
    monthlyToSecondRate,
    subgraphRequest,
    toBN,
    waitUntilBlockIndexed,
} from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IDistributionLocalData,
    IIndex,
    IStreamData,
    IStreamLocalData,
    IIndexSubscription,
    ITokenStatistic,
    ISubscriberDistributionTesterParams,
    IUpdateGlobalObjects,
    IFlowOperator,
    ILightEntity,
} from "./interfaces";
import {
    ALLOW_CREATE,
    FlowActionType,
    IDAEventType,
} from "./helpers/constants";
import {
    testFlowUpdated,
    testModifyIDA,
    testUpdateFlowOperatorPermissions,
} from "./helpers/testers";
import { BaseProvider } from "@ethersproject/providers";
import { fetchTokenAndValidate } from "./validation/hol/tokenValidator";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { globalQueries } from "./queries/globalQueries";

describe("Subgraph Tests", () => {
    let userAddresses: string[] = [];
    let framework: Framework;
    let dai: TestToken;
    let daix: SuperToken;
    let provider = ethers.getDefaultProvider("http://0.0.0.0:8545");
    let initialTotalSupply: string;

    // A set of locally updated variables to compare with data from the Graph.
    // The data in here comes from
    let revisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token
    let periodRevisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token

    let streamData: { [id: string]: IStreamData | undefined } = {}; // id is stream id
    let indexes: { [id: string]: IIndex | undefined } = {}; // id is index id
    let subscriptions: { [id: string]: IIndexSubscription | undefined } = {}; // id is subscription id
    let accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    } = {}; // id is ats id
    let tokenStatistics: { [id: string]: ITokenStatistic | undefined } = {}; // id is tokenStats id
    let flowOperators: { [id: string]: IFlowOperator | undefined } = {}; // id is flowOperator-token-sender

    function updateGlobalObjects(data: IUpdateGlobalObjects) {
        if (data.revisionIndexId && data.updatedStreamData) {
            revisionIndexes[data.revisionIndexId] = Number(
                data.updatedStreamData.revisionIndex
            );
            periodRevisionIndexes[data.revisionIndexId] = Number(
                data.updatedStreamData.periodRevisionIndex
            );
        }
        if (data.updatedStreamData) {
            streamData[data.updatedStreamData.id] = data.updatedStreamData;
        }
        if (data.updatedSenderATS) {
            accountTokenSnapshots[data.updatedSenderATS.id] =
                data.updatedSenderATS;
        }
        if (data.updatedReceiverATS) {
            accountTokenSnapshots[data.updatedReceiverATS.id] =
                data.updatedReceiverATS;
        }
        if (data.updatedIndex) {
            indexes[data.updatedIndex.id] = data.updatedIndex;
        }
        if (data.updatedSubscription) {
            subscriptions[data.updatedSubscription.id] =
                data.updatedSubscription;
        }
        if (data.updatedPublisherATS) {
            accountTokenSnapshots[data.updatedPublisherATS.id] =
                data.updatedPublisherATS;
        }
        if (data.updatedSubscriberATS) {
            accountTokenSnapshots[data.updatedSubscriberATS.id] =
                data.updatedSubscriberATS;
        }
        if (data.updatedFlowOperator) {
            flowOperators[data.updatedFlowOperator.id] =
                data.updatedFlowOperator;
        }
        if (data.updatedTokenStats) {
            tokenStatistics[data.updatedTokenStats.id] = data.updatedTokenStats;
        }
    }

    async function transferAndUpdate(
        amount: string,
        sender: SignerWithAddress,
        receiver: string
    ) {
        let response = await daix
            .transfer({
                receiver,
                amount,
            })
            .exec(sender);
        await response.wait();

        if (!response.blockNumber) {
            throw new Error("No block number.");
        }

        let block = await provider.getBlock(response.blockNumber);
        // update transfer amount
        const senderATSId = getATSId(
            sender.address.toLowerCase(),
            daix.address.toLowerCase()
        );
        const senderATS = accountTokenSnapshots[senderATSId];

        if (senderATS) {
            const updatedTransferAmount = toBN(
                senderATS.totalAmountTransferredUntilUpdatedAt
            ).add(toBN(amount));
            accountTokenSnapshots[senderATSId] = {
                ...senderATS,
                totalAmountTransferredUntilUpdatedAt:
                    updatedTransferAmount.toString(),
            };
        }
        const tokenStats = tokenStatistics[daix.address.toLowerCase()];

        if (tokenStats) {
            const timeDelta = toBN(block.timestamp.toString()).sub(
                toBN(tokenStats.updatedAtTimestamp)
            );
            const streamedAmountSinceUpdatedAt = toBN(
                tokenStats.totalOutflowRate
            ).mul(timeDelta);
            const totalAmountStreamedUntilUpdatedAt = toBN(
                tokenStats.totalAmountStreamedUntilUpdatedAt
            ).add(streamedAmountSinceUpdatedAt);
            tokenStatistics[daix.address.toLowerCase()] = {
                ...tokenStats,
                updatedAtBlockNumber: response.blockNumber!.toString(),
                updatedAtTimestamp: block.timestamp.toString(),
                totalAmountStreamedUntilUpdatedAt:
                    totalAmountStreamedUntilUpdatedAt.toString(),
            };
        }
    }

    function getStreamLocalData(): IStreamLocalData {
        return {
            accountTokenSnapshots,
            flowOperators,
            revisionIndexes,
            periodRevisionIndexes,
            streamData,
            tokenStatistics,
        };
    }

    /**
     * @dev if num is 100 it returns: "100000000000000000000" for BN
     * @param num
     * @returns
     */
    function to18DecimalNumString(num: number) {
        return (num * 10 ** 18).toString();
    }

    function getDistributionLocalData(): IDistributionLocalData {
        return {
            accountTokenSnapshots,
            tokenStatistics,
            indexes,
            subscriptions,
        };
    }

    function getAccountTokenSnapshotsArray() {
        return Object.values(accountTokenSnapshots).filter(
            (x) => x != undefined
        ) as IAccountTokenSnapshot[];
    }

    function getBaseIDAData(
        baseParams: ISubscriberDistributionTesterParams,
        provider: BaseProvider
    ) {
        return {
            framework,
            superToken: daix,
            localData: getDistributionLocalData(),
            baseParams,
            provider,
            atsArray: getAccountTokenSnapshotsArray(),
        };
    }
    function getBaseCFAData(provider: BaseProvider, tokenAddress: string) {
        return {
            framework,
            superToken: daix,
            localData: getStreamLocalData(),
            atsArray: getAccountTokenSnapshotsArray(),
            provider,
            tokenAddress,
        };
    }

    before(async () => {
        // NOTE: make the token symbol more customizable in the future
        let { users, sf, fDAI, fDAIx, totalSupply } = await beforeSetup(
            100000000
        );
        initialTotalSupply = totalSupply;
        userAddresses = users;
        framework = sf;
        dai = fDAI;
        daix = fDAIx;
    });

    describe("Token Tests", () => {
        it("Should return the correct data for the superToken", async () => {
            await fetchTokenAndValidate(
                daix.address.toLowerCase(),
                "Super fDAI",
                "fDAIx",
                true,
                false,
                dai.address,
                18
            );
        });

        it("Should return the correct data for the regularToken", async () => {
            await fetchTokenAndValidate(
                dai.address.toLowerCase(),
                "Fake DAI",
                "fDAI",
                true,
                false,
                "",
                18
            );
        });

        it("Should be able to unlist and relist tokens", async () => {
            const [deployer] = await ethers.getSigners();

            // MUST WAIT until block indexed each time to catch up
            // Unlist fDAIx
            let txn = await framework.contracts.resolver
                .connect(deployer)
                .set("supertokens.test.fDAIx", ethers.constants.AddressZero);

            let receipt = await txn.wait();
            await waitUntilBlockIndexed(receipt.blockNumber);
            await fetchTokenAndValidate(
                daix.address.toLowerCase(),
                "Super fDAI",
                "fDAIx",
                false,
                false,
                dai.address,
                18
            );

            // List fDAIx
            txn = await framework.contracts.resolver
                .connect(deployer)
                .set("supertokens.test.fDAIx", daix.address);
            receipt = await txn.wait();
            await waitUntilBlockIndexed(receipt.blockNumber);
            await fetchTokenAndValidate(
                daix.address.toLowerCase(),
                "Super fDAI",
                "fDAIx",
                true,
                false,
                dai.address,
                18
            );
        });

        it("Should properly set native asset as listed SuperToken", async () => {
            const [deployer] = await ethers.getSigners();
            const ETHx = await framework.loadNativeAssetSuperToken("ETHx");

            // NOTE: we execute a transaction here with ETHx to get the name/symbol indexed on the subgraph
            let txn = await ETHx.upgrade({
                amount: ethers.utils.parseEther("100").toString(),
            }).exec(deployer);
            let receipt = await txn.wait();
            await waitUntilBlockIndexed(receipt.blockNumber);

            await fetchTokenAndValidate(
                ETHx.address.toLowerCase(),
                "Super ETH",
                "ETHx",
                true,
                true,
                ethers.constants.AddressZero,
                18
            );
        });
    });

    describe("ConstantFlowAgreementV1 Tests", () => {
        /**
         * Flow Creation Tests
         */
        it("Should return correct data after creating multiple flows from one person to many.", async () => {
            // Deployer to All
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Create,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                    totalSupply: initialTotalSupply,
                });
                updateGlobalObjects(data);
            }
        });

        it("Should return correct data after creating multiple flows from many to one person.", async () => {
            // All to Deployer
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Create,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    flowOperator: userAddresses[i],
                    receiver: userAddresses[0],
                });
                updateGlobalObjects(data);
            }
        });

        // /**
        //  * Flow Update Tests
        //  */
        it("Should return correct data after updating multiple flows from one person to many.", async () => {
            // Deployer to All
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                });
                updateGlobalObjects(data);
            }

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                });
                updateGlobalObjects(data);
            }
        });

        it("Should return correct data after updating multiple flows from many to one person.", async () => {
            // All to Deployer
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    flowOperator: userAddresses[i],
                    receiver: userAddresses[0],
                });
                updateGlobalObjects(data);
            }

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate

                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    flowOperator: userAddresses[i],
                    receiver: userAddresses[0],
                });
                updateGlobalObjects(data);
            }
        });

        /**
         * Flow Delete Tests
         */
        it("Should return correct data after deleting a created flow.", async () => {
            // delete the updated flows from all to deployer
            for (let i = userAddresses.length; i < userAddresses.length; i++) {
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: 0,
                    sender: userAddresses[i],
                    flowOperator: userAddresses[i],
                    receiver: userAddresses[0],
                });
                updateGlobalObjects(data);
            }
        });

        it("Should return correct data after deleting an updated flow.", async () => {
            // delete the updated flows from deployer to all
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: 0,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                });
                updateGlobalObjects(data);
            }
        });

        it("Should return correct data after creating a flow after deleting.", async () => {
            // recreate the deleted flows from deployer to all
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Create,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                });
                updateGlobalObjects(data);
            }
        });

        it("Should return correct data after updating a flow after deleting.", async () => {
            // update the recreated flows from deployer to all
            const randomFlowRate = getRandomFlowRate(1000);
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                const data = await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[i],
                });
                updateGlobalObjects(data);
            }
        });

        // @note skipping this test as it is flakey and not
        // behaving properly
        it.skip("Should liquidate a stream", async () => {
            const flowRate = monthlyToSecondRate(5000);
            const sender = userAddresses[0];
            const receiver = userAddresses[1];
            const liquidator = userAddresses[2];

            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: flowRate,
                    sender,
                    flowOperator: sender,
                    receiver,
                })
            );
            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Create,
                    newFlowRate: flowRate,
                    sender,
                    flowOperator: sender,
                    receiver,
                })
            );

            // get balance of sender
            const balanceOfSender = await daix.balanceOf({
                account: sender,
                providerOrSigner: provider,
            });
            const senderSigner = await ethers.getSigner(sender);
            const liquidatorSigner = await ethers.getSigner(liquidator);
            const transferAmount = toBN(balanceOfSender)
                // transfer total - 5 seconds of flow
                .sub(toBN((flowRate * 5).toString()))
                .toString();
            await transferAndUpdate(transferAmount, senderSigner, liquidator);
            // wait for flow to get drained
            // cannot use time traveler due to
            // subgraph constraints
            let balanceOf;
            do {
                balanceOf = await daix.realtimeBalanceOf({
                    account: sender,
                    providerOrSigner: provider,
                });
                await asleep(1000);
            } while (Number(balanceOf.availableBalance) >= 0);

            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: 0,
                    sender,
                    flowOperator: liquidator,
                    receiver,
                    liquidator,
                })
            );
            const balanceOfLiquidator = await daix.realtimeBalanceOf({
                account: liquidator,
                providerOrSigner: provider,
            });
            const returnAmount = toBN(balanceOfLiquidator.availableBalance).div(
                toBN(2)
            );

            // transfer balance back to sender
            await transferAndUpdate(
                returnAmount.toString(),
                liquidatorSigner,
                sender
            );
        });

        it("Should be able to update flow operator permissions", async () => {
            const flowRateAllowance = monthlyToSecondRate(5000).toString();

            // give create permissions
            updateGlobalObjects(
                await testUpdateFlowOperatorPermissions({
                    isCreate: true,
                    framework,
                    provider,
                    superToken: daix,
                    isFullControl: false,
                    isFullControlRevoke: false,
                    sender: userAddresses[0],
                    permissions: ALLOW_CREATE,
                    flowOperator: userAddresses[2],
                    flowOperators,
                    flowRateAllowance,
                    accountTokenSnapshots,
                })
            );

            // revoke all permissions
            updateGlobalObjects(
                await testUpdateFlowOperatorPermissions({
                    isCreate: false,
                    framework,
                    provider,
                    superToken: daix,
                    isFullControl: false,
                    isFullControlRevoke: true,
                    sender: userAddresses[0],
                    permissions: 0,
                    flowOperator: userAddresses[2],
                    flowOperators,
                    flowRateAllowance: "0",
                    accountTokenSnapshots,
                })
            );

            // grant full control
            updateGlobalObjects(
                await testUpdateFlowOperatorPermissions({
                    isCreate: false,
                    framework,
                    provider,
                    superToken: daix,
                    isFullControl: true,
                    isFullControlRevoke: false,
                    sender: userAddresses[0],
                    permissions: 0,
                    flowOperator: userAddresses[2],
                    flowOperators,
                    flowRateAllowance: "0",
                    accountTokenSnapshots,
                })
            );
        });

        it("Should allow flowOperator to create/update/delete a flow on behalf of sender", async () => {
            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: 0,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[0],
                    receiver: userAddresses[1],
                })
            );
            // create flow by operator
            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Create,
                    newFlowRate: monthlyToSecondRate(1000),
                    sender: userAddresses[0],
                    flowOperator: userAddresses[2],
                    receiver: userAddresses[1],
                })
            );

            // update flow by operator
            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Update,
                    newFlowRate: monthlyToSecondRate(2000),
                    sender: userAddresses[0],
                    flowOperator: userAddresses[2],
                    receiver: userAddresses[1],
                })
            );

            // delete flow by operator
            updateGlobalObjects(
                await testFlowUpdated({
                    ...getBaseCFAData(provider, daix.address),
                    actionType: FlowActionType.Delete,
                    newFlowRate: 0,
                    sender: userAddresses[0],
                    flowOperator: userAddresses[2],
                    receiver: userAddresses[1],
                })
            );
        });
    });

    /**
     * Note: The goal of the IDA tests are to test out all of the mapping function logic
     * by not always following the happy path, but rather trying out some alternative paths
     * which are unlikely, but still possible.
     * Also, please use the Subscription event type for actions which contain both a subscription
     * and index event type.
     */
    describe("InstantDistributionAgreement Tests", () => {
        /**
         * Create Index Tests
         */
        it("Should return correct data after multiple users create an index", async () => {
            const token = daix.address;
            for (let i = 0; i < userAddresses.length; i++) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher: userAddresses[i],
                    indexId: i,
                    userData: "0x",
                    subscriber: "",
                };
                const data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.IndexCreated,
                });
                updateGlobalObjects(data);
            }
        });

        /**
         * Case #0:
         * 0 units not approved => not approved with units => not approved with units (diff)
         * => distribute units to non-approved with units => delete subscriptions
         * => approve deleted subscriptions => update deleted subscription units to 0
         * => delete approved subscription => update units to great than 0
         * => update units to 0
         */
        it("Should return correct data after handling case #0.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (let i = 1; i < Math.floor(userAddresses.length / 2); i++) {
                const subscriber = userAddresses[i];
                const publisher = userAddresses[0];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher,
                    indexId: 0,
                    userData: "0x",
                    subscriber,
                };

                let units = toBN(to18DecimalNumString(100));

                // update sub units
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                units = toBN(to18DecimalNumString(150));

                // update sub units again
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                // distribute units to non-approved subscribers
                const amountOrIndexValue = toBN((20).toString());

                data = await testModifyIDA({
                    ...getBaseIDAData(
                        { ...baseParams, subscriber: "" },
                        provider
                    ),
                    eventType: IDAEventType.IndexUpdated,
                    amountOrIndexValue,
                    isDistribute: true,
                });
                updateGlobalObjects(data);

                // delete subscriptions
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: false,
                    sender: publisher,
                });
                updateGlobalObjects(data);

                // approve deleted sub
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                // update sub units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);

                // delete subscriptions
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: false,
                    sender: publisher,
                });
                updateGlobalObjects(data);

                // update sub units to > 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(100)),
                });
                updateGlobalObjects(data);

                // update sub units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);
            }
        });

        /**
         * Case #1:
         * 0 units not approved => not approved with units => approved with units
         * => update approved subscription with units to 0
         * => update approved subscription without units to 0
         * => revoke approved subscription without units
         * => update revoked subscription units to greater than 0
         * => approve revoked subscription with units
         * => revoke approved subscription with units
         * => update subscription units on revoked subscription with units
         * => update revoke subscription units to 0
         * => update revoke subscription to 0 (again)
         */
        it("Should return correct data after handling case #1.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher: userAddresses[1],
                    indexId: 1,
                    userData: "0x",
                    subscriber,
                };

                let units = toBN(to18DecimalNumString(100));

                // update sub units
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                // approve sub w/ units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                units = toBN((0).toString());

                // update approved sub units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                // update sub units from 0 to 0 (we should be able to do this technically)
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                // revoke approved sub w/o units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: true,
                    sender: subscriber,
                });
                updateGlobalObjects(data);

                // update revoked sub units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(150)),
                });
                updateGlobalObjects(data);

                // approve revoked sub w/ units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                // revoke approved sub w/ units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: true,
                    sender: subscriber,
                });
                updateGlobalObjects(data);

                // update sub units on revoked sub w/ units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(175)),
                });
                updateGlobalObjects(data);

                // update revoked sub w/ units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);
                // update revoked sub w/o units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);
            }
        });

        /**
         * Case #2:
         * 0 units not approved => approved w/o units => approved with units
         * => approved w/ units => distribute units to approved subscribers
         * => delete approved subscriptions who received distribution
         * => update deleted subscription's units to 0
         * => update not approved subscriptions w/o units to 0 from 0
         * => approve not approved subscription w/o units
         * => update approved subscriptions units to greater than 0
         */
        it("Should return correct data after approve sub without units, update sub units, distribute units and delete sub", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const publisher = userAddresses[2];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher,
                    indexId: 2,
                    userData: "0x",
                    subscriber,
                };

                // approve subscriptions
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                // update sub units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(100)),
                });
                updateGlobalObjects(data);

                // update sub units
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(150)),
                });
                updateGlobalObjects(data);

                // distribute units to approved subscribers
                const amountOrIndexValue = toBN(to18DecimalNumString(100));
                data = await testModifyIDA({
                    ...getBaseIDAData(
                        { ...baseParams, subscriber: "" },
                        provider
                    ),
                    eventType: IDAEventType.IndexUpdated,
                    amountOrIndexValue,
                    isDistribute: true,
                });
                updateGlobalObjects(data);

                // delete subs
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: false,
                    sender: publisher,
                });
                updateGlobalObjects(data);

                // update sub units to 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);

                // update not approved sub w/o units to 0 again
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN((0).toString()),
                });
                updateGlobalObjects(data);

                // approve subscriptions
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                // update approved sub w/o units to > 0
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units: toBN(to18DecimalNumString(150)),
                });
                updateGlobalObjects(data);
            }
        });

        /**
         * Case #3:
         * update from 0 units not approved => approve with units for half and
         * update from 0 units not approved => not approved with units for the other half
         * distribute units to approved + not approved users
         * non approved users claim pending tokens
         */
        it("Should return correct data after non-subscribers get units, claim them and publisher updates their units to 0.", async () => {
            const token = daix.address;
            const multiBaseParams = {
                provider,
                token,
                publisher: userAddresses[3],
                indexId: 3,
                atsArray: getAccountTokenSnapshotsArray(),
                userData: "0x",
            };

            // approve and update sub units for half
            for (let i = 0; i < Math.floor(userAddresses.length / 2); i++) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // approve subscriptions
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionApproved,
                });
                updateGlobalObjects(data);

                // update sub units
                let units = toBN(to18DecimalNumString(100));
                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                units = toBN(to18DecimalNumString(150));

                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);
            }

            // update sub units for the other half
            for (
                let i = Math.floor(userAddresses.length / 2);
                i < userAddresses.length;
                i++
            ) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // update sub units
                let units = toBN(to18DecimalNumString(100));
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);

                units = toBN(to18DecimalNumString(150));

                data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });
                updateGlobalObjects(data);
            }

            // distribute units to pending + claimed users
            const amountOrIndexValue = toBN(to18DecimalNumString(200));
            let data = await testModifyIDA({
                ...getBaseIDAData(
                    { ...multiBaseParams, subscriber: "" },
                    provider
                ),
                eventType: IDAEventType.IndexUpdated,
                amountOrIndexValue,
                isDistribute: true,
            });
            updateGlobalObjects(data);

            for (
                let i = Math.floor(userAddresses.length / 2);
                i < userAddresses.length;
                i++
            ) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // claim pending units
                let data = await testModifyIDA({
                    ...getBaseIDAData(baseParams, provider),
                    eventType: IDAEventType.SubscriptionDistributionClaimed,
                });
                updateGlobalObjects(data);
            }
        });
    });

    describe("Global Check", () => {
        it("There should be at least one Account or AccountTokenSnapshot entities with the zero address", async () => {
            const accountTokenSnapshotIds = await fetchEntityAndEnsureExistence<
                ILightEntity[]
            >(
                globalQueries.getAccountTokenSnapshotIds,
                ethers.constants.AddressZero,
                "AccountTokenSnapshot"
            );
            const accounts = await fetchEntityAndEnsureExistence<
                ILightEntity[]
            >(
                globalQueries.getAccountIds,
                ethers.constants.AddressZero,
                "Account"
            );

            if (accountTokenSnapshotIds.length === 0) {
                throw new Error(
                    "Invariant broken, no nonzero zero address AccountTokenSnapshot's"
                );
            }

            if (accounts.length === 0) {
                throw new Error(
                    "Invariant broken, no nonzero zero address Account's"
                );
            }
        });

        it("Should properly make queries for all events with a LightEntity", async () => {
            // if any of the requests fails, it will return an object containing a property:
            // errors, and we check and throw inside subgraphRequest if this is the case
            // NOTE: it is possible to create an entity which points to another entity
            // but that other entity may not exist and the error will only arise once the
            // query is made
            let data: any = await subgraphRequest<{
                response: ILightEntity[];
            }>(globalQueries.getEvents);
            console.log("getEvents", data);

            data = await subgraphRequest<{
                response: { stream: ILightEntity }[];
            }>(globalQueries.getFlowUpdatedEventsLightEntities);
            console.log("getFlowUpdatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { flowOperator: ILightEntity }[];
            }>(globalQueries.getFlowOperatorUpdatedEventsLightEntities);
            console.log("getFlowOperatorUpdatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexCreatedEventsLightEntities);
            console.log("getIndexCreatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexDistributionClaimedEventsLightEntities);
            console.log("getIndexDistributionClaimedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexUpdatedEventsLightEntities);
            console.log("getIndexUpdatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexSubscribedEventsLightEntities);
            console.log("getIndexSubscribedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexUnitsUpdatedEventsLightEntities);
            console.log("getIndexUnitsUpdatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { index: ILightEntity }[];
            }>(globalQueries.getIndexUnsubscribedEventsLightEntities);
            console.log("getIndexUnsubscribedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { subscription: ILightEntity }[];
            }>(globalQueries.getSubscriptionApprovedEventsLightEntities);
            console.log("getSubscriptionApprovedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { subscription: ILightEntity }[];
            }>(
                globalQueries.getSubscriptionDistributionClaimedEventsLightEntities
            );
            console.log(
                "getSubscriptionDistributionClaimedEventsLightEntities",
                data
            );

            data = await subgraphRequest<{
                response: { subscription: ILightEntity }[];
            }>(globalQueries.getSubscriptionRevokedEventsLightEntities);
            console.log("getSubscriptionRevokedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { subscription: ILightEntity }[];
            }>(globalQueries.getSubscriptionUnitsUpdatedEventsLightEntities);
            console.log("getSubscriptionUnitsUpdatedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { from: ILightEntity; to: ILightEntity }[];
            }>(globalQueries.getTransferEventsLightEntities);
            console.log("getTransferEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { account: ILightEntity }[];
            }>(globalQueries.getTokenDowngradedEventsLightEntities);
            console.log("getTokenDowngradedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: { account: ILightEntity }[];
            }>(globalQueries.getTokenUpgradedEventsLightEntities);
            console.log("getTokenUpgradedEventsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    inflows: ILightEntity[];
                    outflows: ILightEntity[];
                    subscriptions: ILightEntity[];
                    publishedIndexes: ILightEntity[];
                    sentTransferEvents: ILightEntity[];
                    receivedTransferEvents: ILightEntity[];
                    tokenUpgradedEvents: ILightEntity[];
                    tokenDowngradedEvents: ILightEntity[];
                    accountTokenSnapshots: ILightEntity[];
                }[];
            }>(globalQueries.getAccountsLightEntities);
            console.log("getAccountsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    token: ILightEntity;
                    publisher: ILightEntity;
                    subscriptions: ILightEntity[];
                    indexCreatedEvent: ILightEntity[];
                    indexDistributionClaimedEvents: ILightEntity[];
                    indexUpdatedEvents: ILightEntity[];
                    indexSubscribedEvents: ILightEntity[];
                    indexUnitsUpdatedEvents: ILightEntity[];
                    indexUnsubscribedEvents: ILightEntity[];
                }[];
            }>(globalQueries.getIndexesLightEntities);
            console.log("getIndexesLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    subscriber: ILightEntity;
                    index: ILightEntity;
                    subscriptionApprovedEvents: ILightEntity[];
                    subscriptionDistributionClaimedEvents: ILightEntity[];
                    subscriptionRevokedEvents: ILightEntity[];
                    subscriptionUnitsUpdatedEvents: ILightEntity[];
                }[];
            }>(globalQueries.getIndexSubscriptionsLightEntities);
            console.log("getIndexSubscriptionsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    token: ILightEntity;
                    sender: ILightEntity;
                    receiver: ILightEntity;
                    flowUpdatedEvents: ILightEntity[];
                    streamPeriods: ILightEntity[];
                }[];
            }>(globalQueries.getStreamsLightEntities);
            console.log("getStreamsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    token: ILightEntity;
                    sender: ILightEntity;
                    flowOperatorUpdatedEvents: ILightEntity[];
                }[];
            }>(globalQueries.getFlowOperatorsLightEntities);
            console.log("getFlowOperatorsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    stream: ILightEntity;
                    sender: ILightEntity;
                    receiver: ILightEntity;
                    token: ILightEntity;
                    startedAtEvent: ILightEntity;
                }[];
            }>(globalQueries.getStreamPeriodsLightEntities);
            console.log("getStreamPeriodsLightEntities", data);

            data = await subgraphRequest<{
                response: {
                    account: ILightEntity;
                    token: ILightEntity;
                    flowOperators: ILightEntity[];
                }[];
            }>(globalQueries.getAccountTokenSnapshotsLightEntities);
            console.log("getAccountTokenSnapshotsLightEntities", data);

            data = await subgraphRequest<{
                response: { token: ILightEntity }[];
            }>(globalQueries.getTokenStatisticsLightEntities);
            console.log("getTokenStatisticsLightEntities", data);
        });
    });
});
