import { ethers } from "hardhat";
import { BaseProvider } from "@ethersproject/providers";
import { request, gql } from "graphql-request";
import { Framework } from "@superfluid-finance/sdk-core";
import { IMeta, IIndexSubscription } from "../interfaces";
import { FlowActionType } from "./constants";
import IResolverABI from "../../abis/IResolver.json";
import TestTokenABI from "../../abis/TestToken.json";
import { Resolver, TestToken } from "../../typechain";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";

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
 * @param tokenAmount
 * @returns
 */
export const beforeSetup = async (tokenAmount: number) => {
    const [Deployer, Alpha, Bravo, Charlie] = await ethers.getSigners();
    const signers = [Deployer, Alpha, Bravo, Charlie];
    const signerDict: {[address: string]: SignerWithAddress} = signers.reduce(
        (x, y) => ({...x, [y.address]: y}),
        {}
    );
    const users = signers.map((x) => x.address);
    let totalSupply = 0;
    // names[Bob] = "Bob";
    const sf = await Framework.create({
        networkName: "custom",
        dataMode: "WEB3_ONLY",
        protocolReleaseVersion: "test",
        provider: Deployer.provider!,
        resolverAddress: RESOLVER_ADDRESS,
    });

    const resolver = (await ethers.getContractAt(
        IResolverABI,
        RESOLVER_ADDRESS
    )) as Resolver;

    console.log("\n");
    const fDAIxAddress = await resolver.get("supertokens.test.fDAIx");
    const fDAIx = await sf.loadSuperToken(fDAIxAddress);

    // types not properly handling this case
    const fDAI = new ethers.Contract(
        fDAIx.underlyingToken.address,
        TestTokenABI
    ) as TestToken;

    console.log(
        "Mint fDAI, approve fDAIx allowance and upgrade fDAI to fDAIx for users..."
    );
    const amount = tokenAmount.toFixed(0);

    for (let i = 0; i < signers.length; i++) {
        const stringBigIntAmount = ethers.utils.parseUnits(amount).toString();
        await fDAI
            .connect(signers[0])
            .mint(signers[i].address, stringBigIntAmount);
        await fDAI
            .connect(signers[i])
            .approve(fDAIx.address, stringBigIntAmount);
        await fDAIx
            .upgrade({
                amount: stringBigIntAmount,
            })
            .exec(signers[i]);
        totalSupply += Number(stringBigIntAmount);
    }

    // NOTE: although we already set this in initialization, we need to reset it here to ensure
    // we wait for the indexer to catch up before the tests start
    const txn = await resolver.set("supertokens.test.fDAIx", fDAIx.address);
    const receipt = await txn.wait();
    await waitUntilBlockIndexed(receipt.blockNumber);
    const resolverFDAIxAddress = await resolver.get("supertokens.test.fDAIx");

    if (resolverFDAIxAddress !== fDAIx.address) {
        throw new Error("fDAIx not set properly in resolver.");
    }

    console.log(
        "\n************** Superfluid Framework Setup Complete **************\n"
    );

    return {
        users,
        sf,
        fDAI,
        fDAIx,
        signerDict,
        totalSupply: totalSupply.toString(),
    };
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

export function asleep(ms: number) {
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
 * @param provider
 * @param sf
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

    const signer = await ethers.getSigner(sender);
    // any because it the txn.receipt doesn't exist on
    // Transaction
    const txnResponse =
        actionType === FlowActionType.Create
            ? await sf.cfaV1
                  .createFlow({
                      superToken,
                      sender,
                      receiver,
                      flowRate: newFlowRate.toString(),
                      userData: "0x",
                  })
                  .exec(signer)
            : actionType === FlowActionType.Update
            ? await sf.cfaV1
                  .updateFlow({
                      superToken,
                      sender,
                      receiver,
                      flowRate: newFlowRate.toString(),
                      userData: "0x",
                  })
                  .exec(signer)
            : await sf.cfaV1
                  .deleteFlow({
                      superToken,
                      sender,
                      receiver,
                      userData: "0x",
                  })
                  .exec(signer);

    if (!txnResponse.blockNumber) {
        throw new Error("No block number");
    }

    const block = await provider.getBlock(txnResponse.blockNumber);
    const timestamp = block.timestamp;
    await waitUntilBlockIndexed(txnResponse.blockNumber);

    const {flowRate} = await sf.cfaV1.getFlow({
        superToken,
        sender,
        receiver,
        providerOrSigner: provider,
    });

    return {
        txnResponse,
        timestamp,
        flowRate: toBN(flowRate),
    };
};

export const hasSubscriptionWithUnits = (
    subscriptions: { [id: string]: IIndexSubscription | undefined },
    id: string
) => {
    const subscription = subscriptions[id];
    return subscription != null && toBN(subscription.units).gt(toBN(0));
};
