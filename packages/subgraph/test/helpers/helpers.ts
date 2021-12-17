import { ethers } from "hardhat";
import { ContractReceipt } from "ethers";
import { BaseProvider } from "@ethersproject/providers";
import { request, gql } from "graphql-request";
import SuperfluidSDK from "@superfluid-finance/js-sdk";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { IMeta, IIndexSubscription } from "../interfaces";
import { FlowActionType } from "./constants";
import IResolverABI from "../../abis/IResolver.json";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { ConstantFlowAgreementV1Helper } from "@superfluid-finance/js-sdk/src/ConstantFlowAgreementV1Helper";
import { Resolver } from "../../typechain";

// the resolver address should be consistent as long as you use the
// first account retrieved by hardhat's ethers.getSigners():
// 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266 and the nonce is 0
const RESOLVER_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

/**************************************************************************
 * Test Helper Functions
 *************************************************************************/
/**
 * Before setup function - deploy the framework, get signers, deploy test tokens,
 * initialize framework.
 * @param userAddresses
 * @returns
 */
export const beforeSetup = async (tokenAmount: number) => {
    const [Deployer, Alpha, Bravo, Charlie] = (await ethers.getSigners()).map(
        (x) => x.address
    );
    const userAddresses = [Deployer, Alpha, Bravo, Charlie];
    let totalSupply = 0;
    // names[Bob] = "Bob";
    const sf: Framework = new SuperfluidSDK.Framework({
        web3: (global as any).web3,
        version: "test",
        tokens: ["fDAI"],
        resolverAddress: RESOLVER_ADDRESS,
    });

    console.log("\n");
    await sf.initialize();

    const daix = sf.tokens.fDAIx;

    // types not properly handling this case
    const dai = await (sf.contracts as any).TestToken.at(
        daix.underlyingToken.address
    );

    console.log(
        "Mint fDAI, approve fDAIx allowance and upgrade fDAI to fDAIx for users..."
    );
    const amount = tokenAmount.toFixed(0);

    for (let i = 0; i < userAddresses.length; i++) {
        const stringBigIntAmount = ethers.utils.parseUnits(amount).toString();
        const address = userAddresses[i];
        await dai.mint(address, stringBigIntAmount, {
            from: userAddresses[0],
        });
        await dai.approve(daix.address, stringBigIntAmount, {
            from: address,
        });
        await daix.upgrade(stringBigIntAmount, {
            from: address,
        });
        totalSupply += Number(stringBigIntAmount);
    }
    const resolver = (await ethers.getContractAt(
        IResolverABI,
        RESOLVER_ADDRESS
    )) as Resolver;

    // NOTE: although we already set this in initialization, we need to reset it here to ensure
    // we wait for the indexer to catch up before the tests start
    const txn = await resolver.set("supertokens.test.fDAIx", daix.address);
    const receipt = await txn.wait();
    await waitUntilBlockIndexed(receipt.blockNumber);
    const resolverFDAIxAddress = await resolver.get("supertokens.test.fDAIx");

    if (resolverFDAIxAddress !== daix.address) {
        throw new Error("fDAIx not set properly in resolver.");
    }

    console.log(
        "\n************** Superfluid Framework Setup Complete **************\n"
    );

    return [userAddresses, sf, dai, daix, totalSupply.toString()];
};

export const monthlyToSecondRate = (monthlyRate: number) => {
    const days = 30;
    const hours = days * 24;
    const minutes = hours * 60;
    const seconds = minutes * 60;
    return Math.round((monthlyRate / seconds) * 10 ** 18);
};

// NOTE: + 1 ensures that the flow rate is never 0
export const getRandomFlowRate = (max: number) =>
    Math.floor(Math.random() * max) + 1;

export const getCurrentBlockNumber = async () => {
    const query = gql`
        query {
            _meta {
                block {
                    number
                }
            }
        }
    `;
    const data = await subgraphRequest<IMeta>(query);
    if (!data) return 0;

    return data._meta.block.number;
};

export const subgraphRequest = async <T>(
    query: string,
    variables?: { [key: string]: any }
): Promise<T> => {
    try {
        const response = await request<T>(
            "http://localhost:8000/subgraphs/name/superfluid-test",
            query,
            variables
        );
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};

export const fetchEntityAndEnsureExistence = async <T>(
    query: string,
    id: string,
    entityName: string
) => {
    const vars = {
        id,
    };
    const data = await subgraphRequest<{ response: T }>(query, vars);

    if (data.response == null) {
        throw new Error(entityName + " entity not found.");
    }

    return data.response;
};

export const fetchEventAndEnsureExistence = async <T>(
    query: string,
    transactionHash: string,
    eventName: string
) => {
    const vars = {
        transactionHash,
    };
    const data = await subgraphRequest<{
        response: T[];
    }>(query, vars);
    const event = data.response[0];

    if (!event) {
        throw new Error(eventName + " entity not found.");
    }

    return event;
};

export function toBN(num: any) {
    return ethers.BigNumber.from(num);
}

function asleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

export const waitUntilBlockIndexed = async (txnBlockNumber: number) => {
    let currentBlock: number;
    do {
        currentBlock = await getCurrentBlockNumber();
        await asleep(50);
    } while (txnBlockNumber > currentBlock);
};

export const getCurrentTotalAmountStreamed = (
    streamedSoFar: string,
    currentTime: string,
    lastUpdatedAtTime: string,
    outflowRate: string
) => {
    return toBN(streamedSoFar).add(
        toBN(outflowRate).mul(toBN(currentTime).sub(toBN(lastUpdatedAtTime)))
    );
};

/**************************************************************************
 * Entity ID Getters
 *************************************************************************/

export const getStreamId = (
    sender: string,
    receiver: string,
    token: string,
    revisionIndex: string
) =>
    [
        sender.toLowerCase(),
        receiver.toLowerCase(),
        token.toLowerCase(),
        revisionIndex + ".0",
    ].join("-");

export const getRevisionIndexId = (
    sender: string,
    receiver: string,
    token: string
) =>
    [sender.toLowerCase(), receiver.toLowerCase(), token.toLowerCase()].join(
        "-"
    );

export const getIndexId = (publisher: string, token: string, indexId: string) =>
    [publisher.toLowerCase(), token.toLowerCase(), indexId.toLowerCase()].join(
        "-"
    );

export const getSubscriptionId = (
    subscriber: string,
    publisher: string,
    token: string,
    indexId: string
) =>
    [
        subscriber.toLowerCase(),
        publisher.toLowerCase(),
        token.toLowerCase(),
        indexId.toLowerCase(),
    ].join("-");

/**************************************************************************
 * Modifier Functions
 *************************************************************************/

/**
 * Create/Update/Delete a flow between a sender and receiver.
 * Also waits for the graph to index and also returns the receipt
 * of the txn and data from the blockchain.
 * @param sf
 * @param cfaV1
 * @param actionType
 * @param superToken
 * @param sender
 * @param receiver
 * @param newFlowRate
 * @returns txnReceipt, flow updatedAt (on-chain), flowRate (current on-chain)
 */
export const modifyFlowAndReturnCreatedFlowData = async (
    provider: BaseProvider,
    sf: Framework,
    cfaV1: ConstantFlowAgreementV1,
    actionType: FlowActionType,
    superToken: string,
    sender: string,
    receiver: string,
    newFlowRate: number
) => {
    const actionToTypeStringMap = new Map([
        [FlowActionType.Create, "Create"],
        [FlowActionType.Update, "Update"],
        [FlowActionType.Delete, "Delete"],
    ]);
    console.log(
        `********************** ${actionToTypeStringMap.get(
            actionType
        )} a flow **********************`
    );
    const sfCFA = sf.cfa as ConstantFlowAgreementV1Helper;
    // any because it the txn.receipt doesn't exist on
    // Transaction
    const txn: any =
        actionType === FlowActionType.Create
            ? await sfCFA.createFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : actionType === FlowActionType.Update
            ? await sfCFA.updateFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : await sfCFA.deleteFlow({
                  superToken,
                  sender,
                  flowRate: "0",
                  receiver,
                  by: "",
                  userData: "0x",
                  onTransaction: () => {},
              });

    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp;
    await waitUntilBlockIndexed(receipt.blockNumber);

    const [, flowRate] = await cfaV1.getFlow(superToken, sender, receiver);

    return {
        receipt,
        timestamp,
        flowRate,
    };
};

export const hasSubscriptionWithUnits = (
    subscriptions: { [id: string]: IIndexSubscription | undefined },
    id: string
) => {
    const subscription = subscriptions[id];
    return subscription != null && toBN(subscription.units).gt(toBN(0));
};
